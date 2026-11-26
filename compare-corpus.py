#!/usr/bin/env python3
"""Show what a change does to a corpus, as a page to review.

Usage:

    ./compare-corpus.py [BASE] [--corpus hackage] [--output PAGE]

Run it in the development shell. `BASE` defaults to `origin/master` and is
compared with the working tree. `--corpus` names the corpus as the test
suite does, `hackage`, `tilia` or `ghc-9.14.1-testsuite`, or is `all`. The
page goes to `dist-newstyle/corpus-comparison.html` unless `--output` says
otherwise, and opens in a browser as it is.

With `TILIA_CORPUS_DUMP` set, the test suite writes what each example of the
corpora comes out as, or why it does not, under the directory it names, for
each settings it checks. This script runs it in a clone checked out at
`BASE`, kept in `dist-newstyle/compare-corpus/base` so that a later run
builds only what changed, and in the working tree, and puts every example
whose output differs on one page, with its differences under each settings.
`BASE` must be a revision whose test suite reads `TILIA_CORPUS_DUMP`.
"""

from __future__ import annotations

import argparse
import difflib
import json
import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent
TEMPLATE = ROOT / "compare-corpus.html"
DEFAULT_SETTINGS = "the default settings"
CONDITIONAL = re.compile(r"^\s*#\s*if")


def git(*args: str) -> str:
    return subprocess.run(
        ["git", *args], cwd=ROOT, check=True, capture_output=True, text=True
    ).stdout.strip()


def base_tree(rev: str) -> Path:
    # Kept between runs, so that each builds only what changed since the last.
    # It borrows the objects of this repository, so it has every commit.
    tree = ROOT / "dist-newstyle" / "compare-corpus" / "base"
    if not tree.is_dir():
        git("clone", "--quiet", "--shared", "--no-checkout", str(ROOT), str(tree))
    subprocess.run(
        ["git", "checkout", "--quiet", "--force", "--detach", commit(rev)],
        cwd=tree,
        check=True,
    )
    return tree


def commit(rev: str) -> str:
    # The clone's own refs differ from those of this repository.
    return git("rev-parse", "--verify", f"{rev}^{{commit}}")


def dump(tree: Path, corpus: str, into: Path) -> None:
    match = "Tilia.Corpus" if corpus == "all" else f"Tilia.Corpus/{corpus}/"
    print(f"Formatting the corpus in {tree}", file=sys.stderr)
    # The suite fails wherever the outputs differ from what it records, as
    # they do when a change is compared, so what it says matters only where
    # it wrote nothing.
    run = subprocess.run(
        ["cabal", "test", "--test-option=--match", f"--test-option={match}"],
        cwd=tree,
        env={**os.environ, "TILIA_CORPUS_DUMP": str(into)},
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    if not into.is_dir():
        sys.stderr.write(run.stdout)
        sys.exit(
            f"{tree} wrote no examples: does its test suite read TILIA_CORPUS_DUMP?"
        )


def outputs(root: Path) -> dict[str, Path]:
    return {
        str(path.relative_to(root)): path
        for path in root.rglob("*")
        if path.is_file()
    }


def read(path: Path | None) -> list[str]:
    # The suite formats an example with other settings only where the
    # defaults pass, so one side can have nothing to show.
    if path is None:
        return ["(not formatted with these settings)"]
    return path.read_text(encoding="utf-8").split("\n")


def outcome(path: Path | None) -> str:
    if path is None:
        return "absent"
    return "declined" if path.suffix == ".err" else "formatted"


def compared(base: Path, head: Path) -> list[dict]:
    before, after = outputs(base), outputs(head)
    names = {name.removesuffix(".err") for name in before.keys() | after.keys()}
    files = []
    for name in sorted(names):
        a = before.get(name) or before.get(name + ".err")
        b = after.get(name) or after.get(name + ".err")
        x, y = read(a), read(b)
        if x == y and outcome(a) == outcome(b):
            continue
        hunks: list[list] = []
        for line in difflib.unified_diff(x, y, lineterm="", n=3):
            header = re.match(r"@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@", line)
            if header:
                hunks.append([int(header[1]), int(header[2]), 0, []])
            elif not line.startswith(("---", "+++")):
                hunks[-1][3].append(line)
        for hunk in hunks:
            removed = "".join(l[1:] for l in hunk[3] if l[0] == "-").split()
            added = "".join(l[1:] for l in hunk[3] if l[0] == "+").split()
            hunk[2] = int("".join(removed) != "".join(added))
        files.append(
            {
                "p": name,
                "a": sum(l[0] == "+" for h in hunks for l in h[3]),
                "d": sum(l[0] == "-" for h in hunks for l in h[3]),
                "c": int(any(CONDITIONAL.match(l) for l in x + y)),
                "t": int(any(h[2] for h in hunks) or outcome(a) != outcome(b)),
                "h": hunks,
            }
        )
    return files


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Show what a change does to a corpus, as a page to review."
    )
    parser.add_argument("base", nargs="?", default="origin/master")
    parser.add_argument("--corpus", default="hackage")
    parser.add_argument(
        "--output",
        type=Path,
        default=ROOT / "dist-newstyle" / "corpus-comparison.html",
    )
    args = parser.parse_args()

    base = {"name": args.base, "rev": git("rev-parse", "--short", args.base)}
    head = {
        "name": git("rev-parse", "--abbrev-ref", "HEAD"),
        "rev": git("rev-parse", "--short", "HEAD"),
    }
    if git("status", "--porcelain", "--untracked-files=no"):
        head["rev"] += " with changes"

    with tempfile.TemporaryDirectory(prefix="compare-corpus-") as tmp:
        work = Path(tmp)
        dump(base_tree(args.base), args.corpus, work / "base-dump")
        dump(ROOT, args.corpus, work / "head-dump")

        within = "" if args.corpus == "all" else args.corpus
        settings = sorted(
            (
                p.name
                for p in (work / "head-dump").iterdir()
                if (work / "base-dump" / p.name).is_dir()
            ),
            key=lambda name: (name != DEFAULT_SETTINGS, name),
        )
        data = {
            "corpus": args.corpus,
            "base": base,
            "head": head,
            "settings": [
                {
                    "name": name,
                    "files": compared(
                        work / "base-dump" / name / within,
                        work / "head-dump" / name / within,
                    ),
                }
                for name in settings
            ],
        }

    page = TEMPLATE.read_text(encoding="utf-8").replace(
        "/*DATA*/", json.dumps(data, ensure_ascii=False).replace("</", "<\\/")
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(page, encoding="utf-8")
    for entry in data["settings"]:
        print(f"{entry['name']}: {len(entry['files'])} examples differ", file=sys.stderr)
    print(args.output)


if __name__ == "__main__":
    main()
