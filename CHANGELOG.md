## Unreleased

* Keep explicit braces on empty cases in both single-line and multiline layouts.
* Discover existing `optional-packages` in Cabal projects.
* Choose component settings using declared modules and entry points.
* Inherit component settings for conditional source directories.
* Compare formatted CPP configurations under matching branch choices rather
  than matching the order or number of distinct source strings.
* Preserve error-only CPP alternatives without parsing them as Haskell.
* Exclude the files and directories named by a project's `.tiliaignore`.
* Keep the whitespace a line of a multi-line string literal ends in.
* Do not fail on a `SPECIALIZE` expression with no head variable.
* Put no space between a record and its braces on one line, as in
  `T{a = 1}`, `x{a = 1}`, and `data T = T{a :: Int}`.

## Tilia 0.0.1.0

* Initial release.
