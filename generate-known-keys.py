#!/usr/bin/env python3
"""Regenerate src/Tilia/Fixity/KnownKeys/GhcNNN.hs for one GHC series.

Usage:

    ./generate-known-keys.py --ghc /path/to/ghc-9.12.4/bin/ghc

`--ghc` defaults to whatever `ghc` is on PATH. The compiler needs no project
or package environment.

GHC writes some names into interface files as keys rather than spelling them
out, and only its own tables say which name a key stands for. This script
builds a small program against the compiler's own `ghc` package, which prints
every key and the name it stands for, and writes them out for the series that
compiler belongs to. It writes out the fixities of the primitive operations
too, which GHC keeps in a module it has no interface file for, as GHCi
reports them.
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

DUMPER = r"""
module Main (main) where

import Data.Char (ord)
import Data.List (isPrefixOf)
import GHC.Builtin.PrimOps (allThePrimOps, primOpOcc)
import GHC.Builtin.Utils (knownKeyNames)
import GHC.Types.Id.Make (seqId)
import GHC.Types.Name (Name, getName, getOccName, nameModule_maybe, nameOccName, nameUnique)
import GHC.Types.Name.Occurrence (isTcClsNameSpace, isTvNameSpace, occNameSpace, occNameString)
import GHC.Types.Unique (unpkUnique)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Utils.Outputable (alwaysQualify, mkDumpStyle, ppr, showSDocUnsafe, withPprStyle)

main :: IO ()
main = do
  mapM_ key knownKeyNames
  let prim = maybe "" (moduleNameString . moduleName) (nameModule_maybe (getName seqId))
  mapM_ (\o -> putStrLn ("prim\t" <> prim <> "\t" <> occNameString o)) $
    getOccName seqId : fmap primOpOcc allThePrimOps

key :: Name -> IO ()
key n = case nameModule_maybe n of
  Nothing -> pure ()
  Just m ->
    let qualified =
          (moduleNameString (moduleName m) <> ".")
            `isPrefixOf` showSDocUnsafe (withPprStyle (mkDumpStyle alwaysQualify) (ppr n))
     in putStrLn . concat $
          [ "key\t",
            show (ord tag),
            "\t",
            show index,
            "\t",
            moduleNameString (moduleName m),
            "\t",
            if isTcClsNameSpace space || isTvNameSpace space then "t" else "v",
            "\t",
            if qualified then "q" else "u",
            "\t",
            showSDocUnsafe (ppr n)
          ]
  where
    (tag, index) = unpkUnique (nameUnique n)
    space = occNameSpace (nameOccName n)
"""

# What GHCi answers `:info` with for a name that has a fixity.
FIXITY = re.compile(r"^infix([lr]?) (\d+) `?(\S+?)`?$")
# A name that is written as it is rather than in parentheses.
IDENT = re.compile(r"[A-Za-z_][A-Za-z0-9_']*#*")

Key = tuple[int, int, str, str, bool, str]


def run(argv: list[str]) -> str:
    proc = subprocess.run(argv, capture_output=True, text=True)
    if proc.returncode != 0:
        sys.exit(f"{' '.join(argv)} failed:\n{proc.stderr}")
    return proc.stdout


def series_of(ghc: Path) -> tuple[str, str]:
    """The series and the full version of the compiler."""
    version = run([str(ghc), "--numeric-version"]).strip()
    major, minor = version.split(".")[:2]
    return f"{major}{int(minor):02d}", version


def dump(ghc: Path) -> tuple[list[Key], str, list[str]]:
    """Every key the compiler knows with the name it stands for, the module
    of primitive operations, and the names in it."""
    with tempfile.TemporaryDirectory() as tmp:
        source = Path(tmp) / "Dump.hs"
        source.write_text(DUMPER)
        program = Path(tmp) / "dump"
        run(
            [
                str(ghc),
                "-O0",
                "-hide-all-packages",
                "-package",
                "base",
                "-package",
                "ghc",
                "-outputdir",
                tmp,
                "-o",
                str(program),
                str(source),
            ]
        )
        out = run([str(program)])
    keys = set()
    prim_module = ""
    prim = []
    for row in out.splitlines():
        fields = row.split("\t")
        if fields[0] == "key":
            _, tag, index, module, namespace, qualified, name = fields
            keys.add((int(tag), int(index), module, namespace, qualified == "q", name))
        elif fields[0] == "prim":
            _, prim_module, name = fields
            prim.append(name)
    seen: dict[tuple[int, int], Key] = {}
    for entry in keys:
        known = seen.setdefault(entry[:2], entry)
        if known != entry:
            sys.exit(f"key {chr(entry[0])} {entry[1]} stands for both {known} and {entry}")
    return sorted(keys), prim_module, prim


def prim_fixities(ghc: Path, module: str, names: list[str]) -> list[tuple[str, str, int]]:
    """The fixities GHCi reports for the names of the module of primitive
    operations, which has no interface file to read them out of."""
    ghci = ghc.with_name(ghc.name.replace("ghc", "ghci", 1))
    script = f":module {module}\n" + "".join(
        f":info {name if IDENT.fullmatch(name) else '(' + name + ')'}\n" for name in names
    )
    proc = subprocess.run(
        [
            str(ghci),
            "-v0",
            "-ignore-dot-ghci",
            "-XNoImplicitPrelude",
            "-XMagicHash",
            "-XUnboxedTuples",
            "-package",
            "ghc-prim",
            "-package",
            "ghc-internal",
        ],
        input=script,
        capture_output=True,
        text=True,
    )
    wanted = set(names)
    found = {}
    for line in proc.stdout.splitlines():
        match = FIXITY.match(line)
        if match and match.group(3) in wanted:
            found[match.group(3)] = (match.group(1), int(match.group(2)))
    return sorted((name, direction, precedence) for name, (direction, precedence) in found.items())


def haskell_string(s: str) -> str:
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def haskell_char(c: str) -> str:
    return "'\\''" if c == "'" else "'\\\\'" if c == "\\" else f"'{c}'"


DIRECTION = {"l": "LeftAssoc", "r": "RightAssoc", "": "NoAssoc"}


def render(
    series: str,
    version: str,
    entries: list[Key],
    prim_module: str,
    fixities: list[tuple[str, str, int]],
) -> str:
    rows = []
    for tag, index, module, namespace, qualified, name in entries:
        space = "InTypes" if namespace == "t" else "InTerms"
        printed = f"Just {haskell_string(module)}" if qualified else "Nothing"
        rows.append(f"({haskell_char(chr(tag))}, {index}, {space}, {printed}, {haskell_string(name)})")
    body = "\n".join(
        ("  [ " if i == 0 else "    ") + row + ("," if i < len(rows) - 1 else "")
        for i, row in enumerate(rows)
    )
    declared = "\n".join(
        ("    [ " if i == 0 else "      ")
        + f"({haskell_string(name)}, Fixity {DIRECTION[direction]} {precedence})"
        + ("" if i == len(fixities) - 1 else ",")
        for i, (name, direction, precedence) in enumerate(fixities)
    )
    dotted = f"{series[0]}.{int(series[1:])}"
    return f"""{{-# LANGUAGE OverloadedStrings #-}}

-- | The names GHC {dotted} writes into interface files as keys, and the
-- fixities of its primitive operations.
--
-- Generated by @generate-known-keys.py@ in the root of the repository, from
-- GHC {version}. Run that script again to update this table.
module Tilia.Fixity.KnownKeys.Ghc{series}
  ( knownKeys,
    primops,
  )
where

import Data.Text (Text)
import Tilia.Fixity (Direction (..), Fixity (..), Namespace (..))

-- | Every name a key stands for: the tag and index of its key, its
-- namespace, the module GHC prints it with, if it prints one, and its name.
knownKeys :: [(Char, Int, Namespace, Maybe Text, Text)]
knownKeys =
{body}
  ]

-- | The module GHC keeps the primitive operations in, which has no
-- interface file, and the fixities declared there.
primops :: (Text, [(Text, Fixity)])
primops =
  ( {haskell_string(prim_module)},
{declared}
    ]
  )
"""


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--ghc", default="ghc", help="the compiler (default: ghc on PATH)")
    args = parser.parse_args()
    # Not resolved: a wrapper around the compiler is what carries its
    # package environment, under Nix in particular.
    ghc = Path(shutil.which(args.ghc) or args.ghc)
    series, version = series_of(ghc)
    entries, prim_module, prim = dump(ghc)
    fixities = prim_fixities(ghc, prim_module, prim)
    target = Path(__file__).resolve().parent / "src" / "Tilia" / "Fixity" / "KnownKeys" / f"Ghc{series}.hs"
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(render(series, version, entries, prim_module, fixities))
    print(f"wrote {len(entries)} names and {len(fixities)} fixities for GHC {version} to {target}")


if __name__ == "__main__":
    main()
