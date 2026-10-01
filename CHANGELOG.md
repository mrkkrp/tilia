## Unreleased

* Achieve full idempotence and correct handling of comments in all cases
  when CPP is involved. [Issue 6](https://github.com/mrkkrp/tilia/issues/6)
  and [Issue 7](https://github.com/mrkkrp/tilia/issues/7).
* Put a comment that comes out right above a CPP directive at the margin.
  [PR 34](https://github.com/mrkkrp/tilia/pull/34).
* Format a module in which a conditional has an alternative that holds
  nothing but `#error`, rather than declining it as having conditionals that
  do not nest. [PR 34](https://github.com/mrkkrp/tilia/pull/34).
* Do not repeat an expression in every alternative of a conditional when the
  configurations lay it out differently around the part that varies. [PR
  34](https://github.com/mrkkrp/tilia/pull/34).
* Speed up diffing that `tilia check` performs. [PR
  37](https://github.com/mrkkrp/tilia/pull/37).
* Read `.tiliaignore` files the way Git reads `.gitignore` files, with
  globs, negation, and a `.tiliaignore` in any directory of the project.
  Leading whitespace and a leading `./` are no longer disregarded. [PR
  41](https://github.com/mrkkrp/tilia/pull/41).
* Keep a conditional directive that goes on to the next line with a
  backslash whole, rather than read the lines it goes on to as code. [PR
  39](https://github.com/mrkkrp/tilia/pull/39).
* Indent an alternative of a conditional that continues a line further than
  that line, so that an operator starting it does not start a statement.
  [PR 39](https://github.com/mrkkrp/tilia/pull/39).
* Keep an empty line written above a CPP directive on the last line of a
  branch of a conditional. [PR 44](https://github.com/mrkkrp/tilia/pull/44).
* Work out what a module offers once per run, however many threads ask for
  it at the same time. A run that starts with nothing cached is up to three
  times as fast and needs half the memory. [PR
  46](https://github.com/mrkkrp/tilia/pull/46).

## Tilia 0.0.2.0

* Keep explicit braces on empty cases in both single-line and multiline
  layouts. [PR 13](https://github.com/mrkkrp/tilia/pull/13).
* Discover existing `optional-packages` in Cabal projects. [PR
  13](https://github.com/mrkkrp/tilia/pull/13).
* Where components share a source directory, format a module with the
  settings of the component that declares it. [PR
  13](https://github.com/mrkkrp/tilia/pull/13).
* Inherit component settings for conditional source directories. [PR
  13](https://github.com/mrkkrp/tilia/pull/13).
* Compare formatted CPP configurations under matching branch choices rather
  than matching the order or number of distinct source strings. [PR
  13](https://github.com/mrkkrp/tilia/pull/13).
* Preserve error-only CPP alternatives without parsing them as Haskell. [PR
  13](https://github.com/mrkkrp/tilia/pull/13).
* Exclude the files and directories named by a project's `.tiliaignore`.
  [PR 14](https://github.com/mrkkrp/tilia/pull/14).
* Keep the whitespace a line of a multi-line string literal ends in. [PR
  15](https://github.com/mrkkrp/tilia/pull/15).
* Do not fail on a `SPECIALIZE` expression with no head variable. [PR
  15](https://github.com/mrkkrp/tilia/pull/15).
* Put no space between a record and its braces on one line, as in
  `T{a = 1}`, `x{a = 1}`, and `data T = T{a :: Int}`. [Issue
  17](https://github.com/mrkkrp/tilia/issues/17).
* Format only the modules components declare rather than every Haskell
  file under their source directories. [Issue
  3](https://github.com/mrkkrp/tilia/issues/3).
* Format a package's `Setup.hs` with the compiler's default extensions. [PR
  20](https://github.com/mrkkrp/tilia/pull/20).
* Read the interfaces of dependencies concurrently, and each one once. [PR
  23](https://github.com/mrkkrp/tilia/pull/23).
* Read re-exported operators that contain a vertical bar, such as `.|.`. [PR
  24](https://github.com/mrkkrp/tilia/pull/24).
* Read the exports and fixities of dependencies straight out of their
  interface files rather than through `ghc --show-iface`. [PR
  24](https://github.com/mrkkrp/tilia/pull/24).
* Add `--no-cache` to neither read from nor write to the cache. [PR
  25](https://github.com/mrkkrp/tilia/pull/25).
* Add `--no-downloads` to not download the sources of dependencies that are
  missing. [PR 26](https://github.com/mrkkrp/tilia/pull/26).
* Add `--must-not-decline` to turn declined files into failures. [PR
  27](https://github.com/mrkkrp/tilia/pull/27).
* Add `--build-plan` to trust a given build plan as up to date rather than
  have Cabal solve one. [Issue 4](https://github.com/mrkkrp/tilia/issues/4).

## Tilia 0.0.1.0

* Initial release.
