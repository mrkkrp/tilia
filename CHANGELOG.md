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
* Walk a module's syntax tree once rather than five times, which makes
  formatting about 10% faster. [PR
  47](https://github.com/mrkkrp/tilia/pull/47).
* Parse each module that operators are looked up in once a run, read each
  source tarball once a run, and cache module summaries for local modules
  between runs. A run is a fifth to a quarter faster. [PR
  48](https://github.com/mrkkrp/tilia/pull/48).
* Read the operators that interface files refer to by key, rather than run
  `ghc --show-iface` on every interface that refers to one. A run with
  `--no-cache` no longer starts `ghc` and is about a third faster. [PR
  49](https://github.com/mrkkrp/tilia/pull/49).
* Place comments and group declarations in time that does not grow with
  the square of the size of a module. Rendering a 470 KB module with many
  comments takes a second rather than 14. [PR
  50](https://github.com/mrkkrp/tilia/pull/50).
* When declining a file over an operator its imports bring in with
  different fixities, name each fixity and the imports that bring it. [PR
  52](https://github.com/mrkkrp/tilia/pull/52).
* Take hidden items into account in whole-module re-exports. Before this
  change, an operator the import hides could be taken for one the module
  reexports, which declined files as ambiguous and could give an operator
  the wrong fixity. [PR 52](https://github.com/mrkkrp/tilia/pull/52).
* Employ formatting by fragments in the CPP pipeline, so that a module with
  many conditionals no longer runs out of configurations to format. 12 of
  the 15 Hackage corpus modules declined for that reason now format, among
  them QuickCheck's `Test.QuickCheck.Arbitrary` and lens's
  `Language.Haskell.TH.Lens`. [PR
  54](https://github.com/mrkkrp/tilia/pull/54).
* Add `tilia for-editor FILE`, to facilitate editor integrations.
  `Tilia.Editor` offers the same to programs that use Tilia as a library.
  [PR 51](https://github.com/mrkkrp/tilia/pull/51).
* Sort an import whose clause is behind a CPP conditional, such as a
  `hiding` clause behind `#if`, among the other imports, rather than sorting
  the imports either side of the conditional apart, which a second pass then
  sorted again. [PR 55](https://github.com/mrkkrp/tilia/pull/55).
* Keep a comma with the item it follows when the item varies across
  configurations, rather than put it on a line of its own after the
  `#endif`. Several items that vary under one conditional now stay in one
  conditional. [PR 56](https://github.com/mrkkrp/tilia/pull/56).
* Put a conditional around an argument that only some configurations pass
  first, rather than around it and every argument after it, which then
  came out once in each branch. [PR
  57](https://github.com/mrkkrp/tilia/pull/57).
* Keep an empty line written under a comment that carries on a trailing
  comment on the line above. [Issue
  63](https://github.com/mrkkrp/tilia/issues/63).
* Keep what is written after `#else` and `#endif`, such as a comment naming
  the condition. [Issue 61](https://github.com/mrkkrp/tilia/issues/61).
* Print an import list on one line when its parentheses were written on one
  line. A list merged out of several is broken if any of them was written
  across lines. The same goes for the names given with a type, as in `Maybe
  (Just, Nothing)`, in import and export lists. [PR
  73](https://github.com/mrkkrp/tilia/pull/73).
* Work out the fixities in scope of a CPP module whose branches do not
  parse together from each configuration, rather than format the whole
  module without fixities and without checking for operators of unknown
  fixity. Formatting often gives a module such branches, so a second pass
  could lay out its operators differently. [Issue
  60](https://github.com/mrkkrp/tilia/issues/60).
* Merge imports of one module that hide names only when they hide the same
  names, since imports hiding different names bring in more together than
  one hiding all of them would. [PR
  76](https://github.com/mrkkrp/tilia/pull/76).
* Keep a name in a `hiding` list apart from the same name with its own
  parentheses, rather than fold them together, since alone it also hides
  any data constructor of that name. [PR
  76](https://github.com/mrkkrp/tilia/pull/76).
* Keep a comment written after an operator at the end of a line with the
  operand before the operator, rather than move it after the next operand
  when the operator goes to the start of the next line. [Issue
  62](https://github.com/mrkkrp/tilia/issues/62).
* Take the macros of other compilers, `__MHS__` and `__HUGS__`, as not
  defined under a build plan for GHC, so that what only those compilers see,
  such as an import of a module GHC does not have, is not in scope. [Issue
  70](https://github.com/mrkkrp/tilia/issues/70).
* Keep a conditional nested where it was written when a conditional asking
  the same question holds a `LANGUAGE` pragma, rather than pull it outside
  the conditional around it and copy what that one holds into both of its
  branches. [Issue 64](https://github.com/mrkkrp/tilia/issues/64).
* Keep a comment written before the closing bracket of a list
  comprehension, an arithmetic sequence or a Template Haskell quote inside
  the brackets, rather than move it after them or, when nothing follows,
  out of the declaration. [PR 79](https://github.com/mrkkrp/tilia/pull/79).
* Put a conditional around an argument that only some configurations pass
  last, rather than around it and the argument before it, which then came
  out once more under the `#else` of a second conditional. [Issue
  66](https://github.com/mrkkrp/tilia/issues/66).
* Put the comma before items at the end of a list that only some
  configurations have on the line of the first of them, rather than on a
  line of its own, and keep several such conditionals in a row apart as
  written rather than nest them and repeat the items of one in both
  branches of the other. [Issue 69](https://github.com/mrkkrp/tilia/issues/69).
* Do not blame an operator on a module that could not be read where it
  cannot have come from there: a name the module defines itself, one a
  local binding captures, or one an import that could be read brings in.
  A module that hands a name on is no longer held up by an unreadable
  import that could not have supplied it, such as one under another
  qualifier. [Issue 67](https://github.com/mrkkrp/tilia/issues/67).
* Give an operator a local binding captures the fixity its binding group
  declares, or `infixl 9`, rather than that of an import of the same name.
  [Issue 67](https://github.com/mrkkrp/tilia/issues/67).
* Take a name that one of the project's own modules, or a dependency read
  from its source, brings in as settling the name's fixity despite an
  unreadable import, as one read from an interface file already did. [Issue
  84](https://github.com/mrkkrp/tilia/issues/84).
* Settle each name a module re-exports individually, rather than give up on
  all of them because one could have come from a module that cannot be read.
  [Issue 86](https://github.com/mrkkrp/tilia/issues/86).
* Give `:` its fixity wherever it is written, rather than only where the
  `Prelude` is in scope.
* Take the fixities of the packages that come with the compiler from the
  compiler's interface files, rather than from a table written for one
  version of it, and read a module one package exposes but another holds,
  such as `GHC.Num.Integer`, as the module it stands for.
* Read every name an interface file refers to by key, so that a name such as
  `fmap` settles a use of it despite an unreadable import, as other names
  already did.
* Take a constructor, field or method that an import list brings in through
  `T(..)` or `T(a, b)` as settling the name's fixity despite an unreadable
  import, as a variable named on its own already did.
* Keep a comment apart from a Haddock under it only where it comes out
  right above one, rather than in every branch of a conditional it is
  printed in, which a second pass then undid. [Issue
  82](https://github.com/mrkkrp/tilia/issues/82).
* Lay out a module with one declaration the way one with several is laid
  out, so that where a conditional changes how many declarations there are,
  the code after it is not repeated under both branches of another
  conditional. [Issue 82](https://github.com/mrkkrp/tilia/issues/82).
* Keep a module header with a `DEPRECATED` or `WARNING` pragma written on a
  line of its own on several lines when it has no export list, rather than
  join it onto one line. [Issue 96](https://github.com/mrkkrp/tilia/issues/96).
* Put a comment written right after another one where that one goes, rather
  than with what follows, so that a second comment between a name and its
  `=` no longer moves after the `=`. [Issue
  94](https://github.com/mrkkrp/tilia/issues/94).
* Leave the empty line between the header and the imports where it is when
  sorting moves the first import further down, rather than carry it into
  the imports with a comment written right on top of that import. [Issue
  92](https://github.com/mrkkrp/tilia/issues/92).
* Keep a comment that an empty line sets apart from the first import, such
  as a heading over all the imports, at the top of the imports when sorting
  moves that import further down, rather than move it along. [Issue
  92](https://github.com/mrkkrp/tilia/issues/92).
* Put a directive written under the empty line that ends a `#define`
  going on with a backslash under that empty line too, rather than right
  under the `#define`, where it became part of the definition. [PR
  111](https://github.com/mrkkrp/tilia/pull/111).
* Put a line that holds nothing but a use of a function-like macro the
  module defines, such as `WITNESSES(:: [Witness])` among the fields of a
  record, back as it was written, rather than read it as code. A module
  such as `Test.QuickCheck.Property` is now formatted, and the fixities it
  declares are read from its source rather than written into Tilia. [PR
  111](https://github.com/mrkkrp/tilia/pull/111).
* Leave out configurations that no definition of the macros gives, such as
  one taking the branch of `#if X` but no branch of `#ifdef X`, or one
  taking the branch of `#if MIN_VERSION_base(4,11,0)` but no branch of `#if
  MIN_VERSION_base(4,10,0)`, so that a module turning an extension on under
  one of them and using it under the other is no longer declined over them.
  [Issue 65](https://github.com/mrkkrp/tilia/issues/65).
* Put an empty line after the imports only where declarations follow them,
  so that a branch of a conditional holding only imports no longer ends
  with one where another branch goes on to declarations. [Issue
  113](https://github.com/mrkkrp/tilia/issues/113).
* Do not repeat the declarations that follow a conditional in every one of
  its alternatives when it holds a pragma or an import in one branch and
  declarations in another, or asks the same question as a conditional
  among the declarations. [Issue
  68](https://github.com/mrkkrp/tilia/issues/68).
* Keep a conditional choosing between a `data` and a `newtype` declaration
  whole, rather than split it into one conditional choosing the keyword and
  another choosing the rest, which a second run gave an empty conditional.
  [Issue 68](https://github.com/mrkkrp/tilia/issues/68).
* Keep a comment written right above a member of a class or an instance
  next to the member before it, rather than set it apart with an empty
  line the author did not write. [Issue
  120](https://github.com/mrkkrp/tilia/issues/120).
* Put no space between a block comment and the closing bracket or the
  comma written right after it, as in `(Bool {- already bound -}, Int)`.
  [Issue 122](https://github.com/mrkkrp/tilia/issues/122).
* Keep a section heading together with the comments written right against
  it, such as the lines of a comment listing points that begin with `*`, or
  rules of dashes above and below a heading, rather than put empty lines
  around it. [Issue 119](https://github.com/mrkkrp/tilia/issues/119).
* Keep a comment written under the last line of a binding, a statement or
  a declaration, lined up with the code on that line, under that code
  where what follows begins further left, rather than move it out to what
  follows. [Issue 121](https://github.com/mrkkrp/tilia/issues/121).
* Keep a conditional that begins in a `where` clause and goes on into the
  declarations after it whole, rather than split it in two with the empty
  line between the declarations inside the second, which a second run moved
  above it. [Issue 118](https://github.com/mrkkrp/tilia/issues/118).
* Keep a conditional that begins in the middle of a declaration where it
  was written, rather than copy the part of the declaration before it into
  each branch, where the branches go on into the declarations after it or
  finish it each their own way. [Issue
  117](https://github.com/mrkkrp/tilia/issues/117).
* Format a module in which a conditional begins above the declarations and
  goes on into them a part at a time, as one without such a conditional is,
  rather than vary the whole module one conditional at a time. Such modules
  are formatted in less than half the time, some in a seventh of it.
  [PR 130](https://github.com/mrkkrp/tilia/pull/130).
* Keep a comment written above the `in` of a `let` among the bindings,
  rather than move it to the other side of `in`, above the body. [Issue
  137](https://github.com/mrkkrp/tilia/issues/137).
* Leave out configurations that no definition of the macros gives where the
  answers turn on no extension too, when one of them would not parse, such
  as one taking no branch of `#if !(MIN_VERSION_base(4,16,0))` and the
  branch of `#if !(MIN_VERSION_base(4,14,0))` around items of an export list
  written with leading commas. [Issue
  133](https://github.com/mrkkrp/tilia/issues/133).
* Put a space between the last item of a list and a block comment written
  after its trailing comma, which formatting drops, as in `(Array, bounds,
  (!) {- assocs -})`, rather than put the comment against the item, which a
  second run set apart. [Issue 132](https://github.com/mrkkrp/tilia/issues/132).
* Keep a `do` block on the line of the `$` or `=` before it when an
  operator follows the block at the column of its statements, as in `it
  "adds" $ do` with a `` `shouldBe` `` under the block, rather than put the
  block on a line of its own one step further in. [Issue
  136](https://github.com/mrkkrp/tilia/issues/136).

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
