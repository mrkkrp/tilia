## Unreleased

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

## Tilia 0.0.1.0

* Initial release.
