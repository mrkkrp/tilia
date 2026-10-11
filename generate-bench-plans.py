#!/usr/bin/env python3
"""Regenerate bench/bench.plans, the build plans under which the benchmarks
of fixity resolution read packages of the Hackage corpus.

Usage:

    ./generate-bench-plans.py --ghc /path/to/ghc-9.10.3/bin/ghc [RELEASE ...]

`--ghc` defaults to whatever `ghc` is on PATH. It has to be the base compiler
holding only the packages it ships with, since cabal plans the packages a
compiler holds at the versions it holds them: under Nix, a store path rather
than the development shell's `ghc`. The releases default to those the file
names already.

Every release is solved on its own, as a project of its own would be, at the
`index-state` of `cabal.project`, with its test suites and benchmarks where
they solve and without them where they do not, as Tilia solves a project.
"""

from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent
OUTPUT = ROOT / "bench" / "bench.plans"

HEADER = """\
# The build plans under which the benchmarks of fixity resolution read
# packages of the Hackage corpus, each solved by cabal for that package on
# its own. ./generate-bench-plans.py writes it.
#
# compiler  The compiler the plans were solved with, which the benchmarks
#           have to be built with.
#
# Every other line names a package whose modules the benchmarks read, in
# the order they read them, or, indented under it, a package its plan holds.
"""


def run(argv: list[str], cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(argv, cwd=cwd, capture_output=True, text=True)


def checked(argv: list[str], cwd: Path | None = None) -> str:
    proc = run(argv, cwd)
    if proc.returncode != 0:
        sys.exit(f"{' '.join(argv)} failed:\n{proc.stderr}")
    return proc.stdout


def added_packages(ghc: Path) -> list[str]:
    """The packages the compiler's global database holds that it did not
    ship with, which are registered at absolute paths rather than under
    ${pkgroot}."""
    ghc_pkg = ghc.with_name(ghc.name.replace("ghc", "ghc-pkg", 1))
    added = []
    for entry in checked([str(ghc_pkg), "dump", "--global"]).split("\n---\n"):
        name = re.search(r"^name:\s*(\S+)", entry, re.M)
        dirs = re.search(r"^import-dirs:\s*(\S+)", entry, re.M)
        if name and dirs and not dirs.group(1).startswith("${pkgroot}"):
            added.append(name.group(1))
    return added


def index_state() -> str:
    found = re.search(r"^index-state:\s*(\S+)", (ROOT / "cabal.project").read_text(), re.M)
    if not found:
        sys.exit("cabal.project names no index-state")
    return found.group(1)


def written_releases() -> list[str]:
    if not OUTPUT.exists():
        return []
    return [
        line
        for line in OUTPUT.read_text().splitlines()
        if line and not line[0].isspace() and not line.startswith(("#", "compiler "))
    ]


def solve(ghc: Path, state: str, release: str) -> list[str]:
    """Every package the plan of a release holds."""
    with tempfile.TemporaryDirectory() as tmp:
        project = Path(tmp)
        checked(["cabal", "get", f"--index-state={state}", f"--destdir={project}", release])
        (project / "cabal.project").write_text(
            f"packages: {release}\nindex-state: {state}\nwith-compiler: {ghc}\n"
        )
        solving = ["cabal", "build", "all", "--dry-run"]
        if run(solving + ["--enable-tests", "--enable-benchmarks"], project).returncode != 0:
            checked(solving, project)
        plan = json.loads((project / "dist-newstyle" / "cache" / "plan.json").read_text())
    return sorted({f"{unit['pkg-name']}-{unit['pkg-version']}" for unit in plan["install-plan"]})


def render(compiler: str, plans: list[tuple[str, list[str]]]) -> str:
    return (
        HEADER
        + f"\ncompiler {compiler}\n"
        + "".join(
            f"\n{release}\n" + "".join(f"  {package}\n" for package in held)
            for release, held in plans
        )
    )


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--ghc", default="ghc", help="the base compiler (default: ghc on PATH)")
    parser.add_argument("releases", nargs="*", help="the releases to solve (default: those the file names)")
    args = parser.parse_args()
    # Not resolved: a wrapper around the compiler is what carries the
    # packages it was not shipped with, under Nix in particular.
    ghc = Path(shutil.which(args.ghc) or args.ghc)
    added = added_packages(ghc)
    if added:
        sys.exit(f"{ghc} holds packages it did not ship with, such as {added[0]}; pass one that holds only its own")
    releases = args.releases or written_releases()
    if not releases:
        sys.exit("no releases to solve; name them")
    compiler = "ghc-" + checked([str(ghc), "--numeric-version"]).strip()
    state = index_state()
    plans = []
    for release in releases:
        plans.append((release, solve(ghc, state, release)))
        print(f"{release}: {len(plans[-1][1])} packages", file=sys.stderr)
    OUTPUT.write_text(render(compiler, plans))
    print(f"wrote {OUTPUT}", file=sys.stderr)


if __name__ == "__main__":
    main()
