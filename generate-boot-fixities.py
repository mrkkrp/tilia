#!/usr/bin/env python3
"""Regenerate tests/Tilia/BootFixities.hs, the fixities the tests take the
boot packages to export, from a GHC installation.

Usage:

    ./generate-boot-fixities.py --ghc /path/to/ghc-9.14.1/bin/ghc

`--ghc` defaults to whatever `ghc` is on PATH. Nix users can point it at a
store path directly; the compiler needs no project or package environment.
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path

# The boot packages we take fixities from.
PACKAGES = [
    "array",
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "exceptions",
    "filepath",
    "ghc-bignum",
    "ghc-boot-th",
    "ghc-internal",
    "ghc-prim",
    "hpc",
    "mtl",
    "os-string",
    "parsec",
    "pretty",
    "process",
    "stm",
    "template-haskell",
    "text",
    "time",
    "transformers",
    "unix",
]

# How many modules one GHCi process handles before we start a fresh one.
# Every module drags its interface (and its dependencies') into the session
# and nothing is ever released, so an unbounded session grows without limit.
CHUNK = 40

SYMBOL = r"[!#$%&*+./<=>?@\\^|~:-]+"
# The trailing hashes are MagicHash names such as unpackCString#: they read
# as identifiers, not operators, however many operator characters they end
# in.
IDENT = r"[A-Za-z_][A-Za-z0-9_']*#*"

# A qualifier has to be matched explicitly rather than stripped after the
# fact, because "." is itself an operator character: "Data.Function.." is
# the operator "." and not some operator named "..".
QUALIFIER = re.compile(r"[A-Z][A-Za-z0-9_']*\.")
NAME = rf"(?:[A-Z][A-Za-z0-9_']*\.)*({SYMBOL}|{IDENT})"

# Reserved syntax that GHC will nonetheless answer `:info` for, at
# precedences outside 0-9 that no source file can use.
RESERVED = {"->", "=>", "::", "=", "|", "\\", "<-", "..", "@", "~"}

OUTPUT = Path("tests/Tilia/BootFixities.hs")

def run_ghci(ghc: Path, script: str) -> tuple[str, str]:
    """Feed a script to GHCi and return what it wrote to stdout and stderr."""
    ghci = ghc.with_name(ghc.name.replace("ghc", "ghci", 1))
    argv = [
        str(ghci),
        "-v0",
        "-ignore-dot-ghci",
        # Without this GHCi keeps Prelude in scope on top of whatever module
        # we asked for, which makes every operator Prelude also exports
        # ambiguous and answers for the wrong module.
        "-XNoImplicitPrelude",
        # `:info` parses the names it is given, and the boot packages export
        # plenty that only parse with these on.
        "-XMagicHash",
        "-XUnboxedTuples",
        "-XUnboxedSums",
    ]
    for package in PACKAGES:
        argv += ["-package", package]
    proc = subprocess.run(argv, input=script, capture_output=True, text=True)
    return proc.stdout, proc.stderr

# GHCi does not print its prompt when stdin is a pipe, so we emit our own
# markers by shelling out. That only stays in step with GHCi's own output if
# the session's handles are line buffered, hence the preamble.
PREAMBLE = (
    ":module + System.IO\n"
    "System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering\n"
    "System.IO.hSetBuffering System.IO.stderr System.IO.LineBuffering\n"
)

def marker(i: int) -> str:
    """Mark both streams, so that errors can be blamed on a command."""
    return f':!echo "@@ {i}"\n:!echo "@@ {i}" >&2\n'

def split_blocks(out: str, count: int) -> list[list[str]]:
    """Split marked GHCi output into one block of lines per marker."""
    blocks: list[list[str]] = [[] for _ in range(count)]
    current: list[str] | None = None
    for line in out.splitlines():
        m = re.fullmatch(r"@@ (\d+)", line)
        if m:
            current = blocks[int(m.group(1))]
        elif current is not None:
            current.append(line)
    return blocks

def chunked(xs: list, n: int):
    for i in range(0, len(xs), n):
        yield xs[i : i + n]

def exposed_modules(ghc: Path) -> list[str]:
    ghc_pkg = ghc.with_name(ghc.name.replace("ghc", "ghc-pkg", 1))
    modules: set[str] = set()
    for package in PACKAGES:
        proc = subprocess.run(
            [str(ghc_pkg), "field", package, "exposed-modules"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        if proc.returncode != 0:
            sys.exit(f"{ghc_pkg} does not know about the package {package}")
        field = proc.stdout.split(":", 1)[1]
        # A re-export reads "Visible.Name from pkg-1.0:Original.Name"; the
        # visible name is the one an import will mention, so the two tokens
        # naming where it came from are dropped.
        tokens = field.replace(",", " ").split()
        i = 0
        while i < len(tokens):
            if tokens[i] == "from":
                i += 2
                continue
            modules.add(tokens[i])
            i += 1
    return sorted(modules)

def unescape(shown: str) -> str:
    """Undo the quoting GHCi puts around the names `:complete` reports."""
    return re.sub(r"\\(.)", r"\1", shown[1:-1])

# `:module` on its own empties the context, so a module that fails to load
# cannot leave the previous one in scope and answer for it.
def scope(module: str) -> str:
    return f":module\n:module {module}\n"

def exported_names(ghc: Path, modules: list[str]) -> dict[str, list[str]]:
    """List what each module exports, by asking what its scope completes to.

    `:complete` reports every name in scope, spelled both plainly and
    qualified; only the plain spellings are of interest here.
    """
    found: dict[str, list[str]] = {}
    for chunk in chunked(modules, CHUNK):
        script = PREAMBLE
        for i, module in enumerate(chunk):
            script += scope(module) + marker(i) + ':complete repl 100000 ""\n'
        out, _ = run_ghci(ghc, script)
        blocks = split_blocks(out, len(chunk))
        for module, block in zip(chunk, blocks):
            names = set()
            for line in block:
                if not line.startswith('"'):
                    continue  # the "how many of how many" header
                name = unescape(line)
                if not QUALIFIER.match(name):
                    names.add(name)
            found[module] = sorted(names - RESERVED)
    return found

# An alphanumeric name comes back in the backticks it would be written in.
FIXITY = re.compile(rf"^infix([lr]?) (\d) `?{NAME}`?$")

# How many names one `:info` command asks about. GHC abandons the rest of
# the command at the first name it dislikes, so a batch that complained
# about anything is halved and asked again rather than trusted.
BATCH = 32

def read_fixities(
    ghc: Path, exports: dict[str, list[str]]
) -> dict[str, dict[str, tuple[str, int]]]:
    """Collect the fixity declarations GHC reports for each module's scope."""
    table: dict[str, dict[str, tuple[str, int]]] = {m: {} for m in exports}
    for chunk in chunked(sorted(exports), CHUNK):
        # (module, names) pairs still to ask about, shrinking as they fail.
        pending = []
        for module in chunk:
            pending += [(module, b) for b in chunked(exports[module], BATCH)]
        known = {m: set(exports[m]) for m in chunk}
        while pending:
            script = PREAMBLE
            for i, (module, batch) in enumerate(pending):
                query = " ".join(
                    name if re.fullmatch(IDENT, name) else f"({name})"
                    for name in batch
                )
                # The marker goes first so that a module which will not load
                # is blamed on its own batch and not on the one before it.
                script += marker(i) + scope(module) + f":info {query}\n"
            out, err = run_ghci(ghc, script)
            blocks = split_blocks(out, len(pending))
            failed = split_blocks(err, len(pending))
            retry = []
            for (module, batch), block, complaint in zip(pending, blocks, failed):
                # Deprecated modules warn on their way into scope; only a
                # real error means the answer was cut short.
                if any("error:" in line for line in complaint):
                    if len(batch) == 1:
                        continue  # not something `:info` will answer for
                    half = len(batch) // 2
                    retry.append((module, batch[:half]))
                    retry.append((module, batch[half:]))
                    continue
                for line in block:
                    m = FIXITY.match(line)
                    if m and m.group(3) in known[module]:
                        table[module][m.group(3)] = (m.group(1), int(m.group(2)))
            pending = retry
    # An operator GHC said nothing about has no fixity declaration, so it
    # takes the default. Alphanumeric names are only worth listing when they
    # were given a fixity, so they do not get the same treatment.
    for module, names in exports.items():
        for name in names:
            if not re.fullmatch(IDENT, name):
                table[module].setdefault(name, ("l", 9))
    return table

# What GHCi writes a declaration of each kind with. A line opening with one
# of these names the thing it declares, which is how an operator is placed
# in the type namespace.
DECLARES_TYPE = re.compile(
    rf"^(?:type family|type role|type instance|type|data instance|data|newtype|class)\s+(?:\({SYMBOL}\)|{IDENT})"
)
# `type role (:~:) nominal` and `type instance …` are about a type without
# declaring one, but they only ever mention a name that is one.
SUBJECT = re.compile(rf"^(?:[a-z ]+?\s+)?(\({SYMBOL}\)|{IDENT})")
# `(<+>) :: …`, `pattern (:>) :: …` and the method signatures inside a
# class: a name with a type is a term.
SIGNATURE = re.compile(rf"^(?:pattern\s+)?(\({SYMBOL}\)|{IDENT})\s+::")

def declared_names(line: str) -> tuple[str | None, str | None]:
    """What a line of `:info` output declares: a type name, a term name."""
    stripped = line.strip()
    if DECLARES_TYPE.match(stripped):
        keywords = ("type", "family", "role", "instance", "data", "newtype", "class")
        words = stripped.split()
        for word in words[1:]:
            if word not in keywords:
                return word.strip("()"), None
        return None, None
    signature = SIGNATURE.match(stripped)
    if signature:
        return None, signature.group(1).strip("()")
    return None, None

def constructors_in(line: str) -> list[str]:
    """The constructors a data declaration writes out, infix ones included."""
    stripped = line.strip()
    if not (stripped.startswith("data ") or stripped.startswith("newtype ")):
        return []
    _, _, rhs = stripped.partition("=")
    return [word.strip("()") for alternative in rhs.split("|") for word in alternative.split()]

def read_namespaces(
    ghc: Path, table: dict[str, dict[str, tuple[str, int]]]
) -> dict[str, dict[str, str]]:
    """Which namespace each operator in the table belongs to.

    A fixity governs the namespaces the name is declared in, and `:info`
    says which those are: a type operator comes back as a `data` or `type`
    declaration, a value or a pattern synonym as a signature. One that
    cannot be placed governs both, which is what every fixity did before
    any of them were told apart.
    """
    wanted = [(module, op) for module in sorted(table) for op in sorted(table[module])]
    found: dict[str, dict[str, str]] = {m: {} for m in table}
    for chunk in chunked(wanted, CHUNK):
        script = PREAMBLE
        for i, (module, op) in enumerate(chunk):
            name = op if re.fullmatch(IDENT, op) else f"({op})"
            script += marker(i) + scope(module) + f":info {name}\n"
        out, _ = run_ghci(ghc, script)
        for (module, op), block in zip(chunk, split_blocks(out, len(chunk))):
            types = False
            terms = False
            for line in block:
                declared_type, declared_term = declared_names(line)
                if declared_type == op:
                    types = True
                if declared_term == op:
                    terms = True
                if op in constructors_in(line):
                    terms = True
            found[module][op] = (
                "b" if types == terms else "t" if types else "v"
            )
    return found

ASSOC = {"l": "LeftAssoc", "r": "RightAssoc", "": "NoAssoc"}
NAMESPACE = {"t": "[InTypes]", "v": "[InTerms]", "b": "[InTypes, InTerms]"}

def escape(op: str) -> str:
    """Spell an operator as a Haskell string literal."""
    return op.replace("\\", "\\\\").replace('"', '\\"')

def render(
    table: dict[str, dict[str, tuple[str, int]]],
    namespaces: dict[str, dict[str, str]],
    version: str,
) -> str:
    """Write the table out as Tilia formats it.
    """
    entries = []
    for module in sorted(table):
        ops = table[module]
        if not ops:
            entries.append(f'entry "{module}" []')
            continue
        rendered = ", ".join(
            f'("{escape(op)}", {NAMESPACE[namespaces[module].get(op, "b")]}, {ASSOC[d]}, {p})'
            for op, (d, p) in sorted(ops.items())
        )
        entries.append(f'entry\n        "{module}"\n        [{rendered}]')
    body = "\n".join(
        ("    [ " if first else "      ") + entry + ("" if last else ",")
        for entry, first, last in (
            (entry, i == 0, i == len(entries) - 1)
            for i, entry in enumerate(entries)
        )
    )
    return f'''{{-# LANGUAGE OverloadedStrings #-}}

-- | Fixities of the operators the boot packages export, the world the tests
-- format the corpus in.
--
-- Generated by @generate-boot-fixities.py@ in the root of the repository,
-- from GHC {version}. Run that script again to update this table.
module Tilia.BootFixities
  ( bootFixities,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Tilia.Fixity

-- | Every module the boot packages expose, with the operators it exports.
bootFixities :: Map Text Fixities
bootFixities =
  Map.fromList
{body}
    ]
  where
    entry name ops =
      ( name,
        Map.fromList
          [ ((namespace, OpName o), Fixity d p)
          | (o, governs, d, p) <- ops,
            namespace <- governs
          ]
      )
'''

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--ghc",
        default=shutil.which("ghc"),
        help="the ghc to interrogate (default: the one on PATH)",
    )
    parser.add_argument(
        "--output",
        default=None,
        help=f"where to write the module (default: {OUTPUT})",
    )
    args = parser.parse_args()
    if not args.ghc:
        sys.exit("no ghc on PATH; pass --ghc")
    ghc = Path(args.ghc).resolve()
    output = Path(args.output) if args.output else Path(__file__).parent / OUTPUT

    version = subprocess.run(
        [str(ghc), "--numeric-version"], stdout=subprocess.PIPE, text=True
    ).stdout.strip()
    print(f"GHC {version}", file=sys.stderr)

    modules = exposed_modules(ghc)
    print(f"{len(modules)} exposed modules", file=sys.stderr)

    exports = exported_names(ghc, modules)
    print(
        f"{sum(len(v) for v in exports.values())} exported names",
        file=sys.stderr,
    )

    table = read_fixities(ghc, exports)
    print(f"{sum(len(v) for v in table.values())} fixities", file=sys.stderr)

    namespaces = read_namespaces(ghc, table)
    placed = sum(1 for m in namespaces.values() for n in m.values() if n != "b")
    print(
        f"{placed} of them placed in one namespace or the other",
        file=sys.stderr,
    )

    output.write_text(render(table, namespaces, version))
    print(f"wrote {output}", file=sys.stderr)

if __name__ == "__main__":
    main()
