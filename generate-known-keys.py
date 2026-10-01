#!/usr/bin/env python3
"""Regenerate src/Tilia/Fixity/KnownKeys/GhcNNN.hs for one GHC series.

Usage:

    ./generate-known-keys.py --ghc /path/to/ghc-9.12.4/bin/ghc

`--ghc` defaults to whatever `ghc` is on PATH. The compiler needs no project
or package environment, but it needs its boot packages installed: what they
declare is what decides which names go into the table.

GHC writes some names into interface files as keys rather than spelling them
out, and only its own tables say which name a key stands for. Of those names
only a few matter to fixities: the operators with a fixity declared for them
and the types and classes that carry such an operator. This script builds a
small program against the compiler's own `ghc` package, which prints every
key and the name it stands for, keeps the names that matter, and writes them
out for the series that compiler belongs to.
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

DUMPER = r"""
module Main (main) where

import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import GHC.Builtin.PrimOps (allThePrimOps, primOpFixity, primOpOcc)
import GHC.Builtin.Utils (knownKeyNames)
import GHC.Types.Id.Make (seqId)
import GHC.Types.Name (Name, getName, getOccName, nameModule_maybe, nameOccName, nameUnique)
import GHC.Types.Name.Occurrence (isSymOcc, isTcClsNameSpace, isTvNameSpace, occNameSpace, occNameString)
import GHC.Types.Unique (unpkUnique)
import GHC.Unit.Module (moduleName, moduleNameString)
import GHC.Utils.Outputable (alwaysQualify, mkDumpStyle, ppr, showSDocUnsafe, withPprStyle)

main :: IO ()
main = do
  mapM_ key knownKeyNames
  let prim = maybe "" (moduleNameString . moduleName) (nameModule_maybe (getName seqId))
  mapM_ (\o -> putStrLn ("prim\t" <> prim <> "\t" <> occNameString o)) $
    getOccName seqId
      : [ primOpOcc op
        | op <- allThePrimOps,
          isJust (primOpFixity op) || isSymOcc (primOpOcc op)
        ]

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

# A name qualified by the module it comes from, as --show-iface writes one.
QUALIFIED = re.compile(r"^(?:[A-Z][A-Za-z0-9_']*\.)+.")
# One entry of an export list: a name, and what it carries if anything.
EXPORT = re.compile(r"^\s+(\S+?)\|?(?:\{([^}]*)\})?\s*$")

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


def dump(ghc: Path) -> tuple[list[Key], str, set[str]]:
    """Every key the compiler knows with the name it stands for, the module
    of primitive operations, and those of its names that have a fixity.

    That module (GHC.Prim, GHC.Internal.Prim since 9.14) has no interface
    file, so Tilia answers for it out of Builtin.hs, which gives every
    operator a fixity, the default where none is declared. Every operator of
    that module counts as having one here too.
    """
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
    prim = set()
    for row in out.splitlines():
        fields = row.split("\t")
        if fields[0] == "key":
            _, tag, index, module, namespace, qualified, name = fields
            keys.add((int(tag), int(index), module, namespace, qualified == "q", name))
        elif fields[0] == "prim":
            _, prim_module, name = fields
            prim.add(name)
    seen: dict[tuple[int, int], Key] = {}
    for entry in keys:
        known = seen.setdefault(entry[:2], entry)
        if known != entry:
            sys.exit(f"key {chr(entry[0])} {entry[1]} stands for both {known} and {entry}")
    return sorted(keys), prim_module, prim


def interface_files(ghc: Path) -> dict[str, Path]:
    """Where each installed module's interface is."""
    ghc_pkg = ghc.with_name(ghc.name.replace("ghc", "ghc-pkg", 1))
    out = run([str(ghc_pkg), "field", "*", "import-dirs", "--expand-pkgroot", "--simple-output"])
    found: dict[str, Path] = {}
    for line in out.split():
        root = Path(line)
        if root.is_dir():
            for hi in sorted(root.rglob("*.hi")):
                module = ".".join(hi.relative_to(root).with_suffix("").parts)
                found.setdefault(module, hi)
    return found


def read_interface(ghc: Path, hi: Path) -> tuple[set[str], dict[str, list[str]]]:
    """The names a module declares fixities for, and what each type or class
    it defines carries."""
    lines = run([str(ghc), "--show-iface", str(hi)]).splitlines()
    fixities: set[str] = set()
    carried: dict[str, list[str]] = {}
    for i, line in enumerate(lines):
        if line.startswith("fixities "):
            text = line[len("fixities ") :]
            for more in lines[i + 1 :]:
                if not more.startswith(" "):
                    break
                text += " " + more.strip()
            for entry in text.split(","):
                words = entry.split()
                if len(words) == 3:
                    fixities.add(words[2].strip("`"))
        elif line == "exports:":
            for entry in lines[i + 1 :]:
                if not entry.startswith(" "):
                    break
                match = EXPORT.match(entry)
                if match and match.group(2) is not None and not QUALIFIED.match(match.group(1)):
                    carried[match.group(1)] = match.group(2).split()
    return fixities, carried


def relevant(ghc: Path) -> list[Key]:
    """The keys of the names that fixities depend on."""
    keys, prim_module, prim = dump(ghc)
    files = interface_files(ghc)
    modules = sorted({module for _, _, module, _, _, _ in keys if module in files})
    with ThreadPoolExecutor() as pool:
        read = dict(zip(modules, pool.map(lambda m: read_interface(ghc, files[m]), modules)))
    read[prim_module] = (prim, {})

    def matters(module: str, name: str) -> bool:
        if module not in read:
            return False
        fixities, carried = read[module]
        return name in fixities or any(kid in fixities for kid in carried.get(name, []))

    return [entry for entry in keys if matters(entry[2], entry[5])]


def haskell_string(s: str) -> str:
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def haskell_char(c: str) -> str:
    return "'\\''" if c == "'" else "'\\\\'" if c == "\\" else f"'{c}'"


def render(series: str, version: str, entries: list[Key]) -> str:
    rows = []
    for tag, index, module, namespace, qualified, name in entries:
        space = "InTypes" if namespace == "t" else "InTerms"
        printed = f"Just {haskell_string(module)}" if qualified else "Nothing"
        rows.append(f"({haskell_char(chr(tag))}, {index}, {space}, {printed}, {haskell_string(name)})")
    body = "\n".join(
        ("  [ " if i == 0 else "    ") + row + ("," if i < len(rows) - 1 else "")
        for i, row in enumerate(rows)
    )
    dotted = f"{series[0]}.{int(series[1:])}"
    return f"""{{-# LANGUAGE OverloadedStrings #-}}

-- | The names GHC {dotted} writes into interface files as keys that fixities
-- depend on.
--
-- Generated by @generate-known-keys.py@ in the root of the repository, from
-- GHC {version}. Run that script again to update this table.
module Tilia.Fixity.KnownKeys.Ghc{series}
  ( knownKeys,
  )
where

import Data.Text (Text)
import Tilia.Fixity (Namespace (..))

-- | Every operator a key stands for that has a fixity, and every type or
-- class that carries one: the tag and index of its key, its namespace, the
-- module GHC prints it with, if it prints one, and its name.
knownKeys :: [(Char, Int, Namespace, Maybe Text, Text)]
knownKeys =
{body}
  ]
"""


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--ghc", default="ghc", help="the compiler (default: ghc on PATH)")
    args = parser.parse_args()
    # Not resolved: a wrapper around the compiler is what carries its
    # package environment, under Nix in particular.
    ghc = Path(shutil.which(args.ghc) or args.ghc)
    series, version = series_of(ghc)
    entries = relevant(ghc)
    target = Path(__file__).resolve().parent / "src" / "Tilia" / "Fixity" / "KnownKeys" / f"Ghc{series}.hs"
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(render(series, version, entries))
    print(f"wrote {len(entries)} names for GHC {version} to {target}")


if __name__ == "__main__":
    main()
