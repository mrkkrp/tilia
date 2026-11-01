## General guidelines

See `HACKING.md`.

## Docs and comments

When you write docs in source code most of them must be a single sentence
that conveys the essence of the thing you are documenting. Do not be afraid
to use precise technical terms. Prefer them strongly to multi-word plain
English formulations. Each sentence after the first one needs justification
to be included—it must be important and non-trivial information.

Do not write line comments that explain what is going on inside the code,
unless the point they convey is truly important and non-trivial. Code should
dominate comments, not the other way around.

Instead of writing in Claudisms, try to write in a style that is already
used in the codebase.

Write lines that are no longer than 76 characters, if possible. The same
applies to commit messages and especially commit headers—they always have to
be under 76 characters.

## Checking your implementations

Before declaring something done, ask yourself the question: is this the
simplest and irreducible implementation of this logic? Are
simplifications/ablations possible that make the solution more elegant?

## Audits

When asked to perform an audit of the codebase it means that you should scan
it for redundant or obsolete fragments (both in code and documentation), as
well as potential bugs and design flaws. Fan out agents as necessary to
accomplish this.

## Hackage scans

A Hackage scan runs Tilia over packages nobody has reviewed and looks for
what it gets wrong. Unless told otherwise:

1. **Pick 100 packages** as a seeded random sample of the packages whose
   latest release is from the last two years. Take at most two of any
   generated family (`amazonka-`, `gogol-`, `stratosphere-`) and leave out
   packages that need `pkg-config` or a `Configure` build. Give the seed in
   the report.
2. **Build the binary from `origin/master`** and copy it out of
   `dist-newstyle`, so that switching branches does not change what is
   being tested. Name its commit in the report.
3. **Run outside the development shell**, with only the system's GHC and
   Cabal on `PATH` and an `XDG_CACHE_HOME` of the scan's own. What Tilia
   decides depends on the packages `ghc-pkg` sees, and the development
   shell sees far more of them than a user's machine does.
4. **Fetch each package with `cabal get`** and give it a `cabal.project`
   with `packages: .` and `allow-newer: all`. Where that leaves no build
   plan, try again with `packages: .` alone.
5. **Format a copy of each package** with `tilia inplace --check-ast
   --check-idempotence`. Append two empty lines to every `.hs` file first:
   Tilia ends what it writes with exactly one newline, so the files that
   lost the padding are the ones it formatted, and the rest go back as they
   were. Then `tilia check --check-ast --check-idempotence` over the result
   must find nothing to change.

Read the results in this order:

- **Failed checks**, exit 17 (the program changed) or 18 (not idempotent),
  are defects.
- **Declines**, sorted by cause. A parse error in a configuration that no
  run of cpp produces is a defect, while a macro standing for syntax, such
  as `CALLCONV`, is a known decline. A fixity said to differ between two
  modules, or a module that "could not be read", is often a component
  Cabal did not plan, such as a benchmark or a `buildable: False` one.
- **Screens of each output against its input.** Look for:
  - a different number of directives;
  - words that grew, which is copied code, mostly from the CPP merge;
  - words that shrank, which is lost code;
  - words that changed branch, with the conditionals paired in order;
  - new lines over 120 columns;
  - lone keywords or punctuation on a line.

  Known normalisations show up here too: one extension per `LANGUAGE`
  pragma, pragma keywords in capitals, merged imports, and empty `where`s
  dropped.
- **The diffs**, split into hunks that only move line breaks and spaces and
  hunks that change the text. Read every hunk that changes the text against
  the input; comments must stay where they were written.

Reduce each candidate to a few lines in a one-module package outside this
repository, where Tilia's own `.cabal` file does not apply. Format it twice
with `tilia for-editor`, which reports a decline on standard error only.

Before calling it a defect, check that it is none of these:
- intended: a vendored example shows it, the changelog describes it, or an
  issue was closed as not planned;
- known: look through `gh issue list --state all`;
- being fixed in an open PR.

Also check whether it is a regression by trying a binary from before the
latest commits. Verify fixity findings with a fresh cache, since the cache
is not keyed by Tilia's version.

Present the findings before filing anything, most serious first:
1. changed or broken code;
2. wrong fixities;
3. copied code;
4. false declines;
5. moved comments;
6. the cosmetic.

Say what was checked and what is intended. An issue states the problem in
one sentence, gives the input and what it "comes out as", and ends with
"Found in `package-version` `path`". Run every claim in the issue before
posting it, and quote output exactly as it was printed.
