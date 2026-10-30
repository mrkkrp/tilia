## Unreleased

* An empty context comes out as written in a class or a data declaration,
  as in `class () => C a`, and before a constructor, as in
  `forall a. () => C a`, rather than being dropped, as it was already kept
  in a signature and in a GADT constructor.
  [Issue 299](https://github.com/mrkkrp/tilia/issues/299).
* Extensions turned on or off with flags in an `OPTIONS_GHC` pragma, or in
  the older `OPTIONS` pragma, such as `-cpp`, `-XLambdaCase`,
  `-fglasgow-exts` and `-fbang-patterns`, are taken into account, so a
  module that needs them is parsed as GHC parses it.
  [Issue 280](https://github.com/mrkkrp/tilia/issues/280).
* An `OPTIONS` pragma in the header is sorted with the `OPTIONS_GHC`
  pragmas, keeping the order the two are written in among each other, rather
  than being left where it was written.
  [PR 319](https://github.com/mrkkrp/tilia/pull/319).
* An extension that turns another off, as `RebindableSyntax` turns off
  `ImplicitPrelude`, does so where it is turned on, as with GHC, so under
  `RebindableSyntax` the Prelude is not taken to be imported, and an import
  of it sorts among the others, as under `NoImplicitPrelude`.
  [PR 319](https://github.com/mrkkrp/tilia/pull/319).
* The pragmas of a header keep the order they are written in where sorting
  them would leave other extensions in force, as with `NoMonoLocalBinds`
  written above `TypeFamilies`, which turns `MonoLocalBinds` back on, or
  with an option turning off an extension a later `LANGUAGE` pragma turns
  on; and editions, of which GHC takes the last, keep their order among
  each other. [PR 319](https://github.com/mrkkrp/tilia/pull/319).
* An options pragma written twice in a row is kept twice rather than
  written once, since a flag such as `-fplugin-opt` counts each time it is
  given. [PR 322](https://github.com/mrkkrp/tilia/pull/322).
* A pragma written below the `module` line, which GHC does not read, stays
  where it is written rather than being moved above the line, where it
  takes effect, and the module is parsed without what it names, as GHC
  parses it. [Issue 288](https://github.com/mrkkrp/tilia/issues/288).
* `--check-ast` reads the pragmas of the header as GHC reads them, and
  reports a module whose formatting would change the order of the flags of
  its options, its `OPTIONS_HADDOCK` pragmas, or how GHC's own flag reader
  takes them to switch extensions, the edition or the Safe Haskell mode.
  [PR 322](https://github.com/mrkkrp/tilia/pull/322).
* An edition of the language a module names in its header, as in
  `{-# LANGUAGE GHC2024 #-}` or with `-XGHC2024`, takes the place of the
  `default-language` of its component, as with GHC, and what
  `default-extensions` and the module's pragmas switch is switched on top
  of it. [Issue 321](https://github.com/mrkkrp/tilia/issues/321).
* An extension turned off after one that implies it, as `NoExplicitForAll`
  after `RankNTypes`, stays off when the module is parsed, as with GHC,
  rather than being turned back on.
  [PR 323](https://github.com/mrkkrp/tilia/pull/323).
* The `LANGUAGE` pragmas of a header are sorted by name after the
  editions, rather than with the names that turn an extension off, and then
  `ImplicitPrelude` and `CUSKs`, after the rest; where sorting them would
  change what is in force, they keep the order they are written in, as
  before. [PR 323](https://github.com/mrkkrp/tilia/pull/323).
* A `LANGUAGE` pragma holding a comment, as in `, PolyKinds -- for T` on
  one of its lines, is read past the comment, as GHC reads it, and kept as
  written, rather than taking the name and the comment together for a name
  and putting the comment before the `#-}` of a pragma of its own, which GHC
  could not read. [Issue 324](https://github.com/mrkkrp/tilia/issues/324).
* `--debug-fixity` says that the imports disagree about a constructor
  written among types, as a promoted constructor is, where a run declines
  the module over it, rather than showing its fixity settled.
  [Issue 304](https://github.com/mrkkrp/tilia/issues/304).
* `.tiliaignore` files and the cache are read and written as UTF-8 whatever
  the locale, so under `LANG=C` a `.tiliaignore` holding a character
  outside ASCII no longer makes `check` and `inplace` crash, nor
  `for-editor` format what it ignores, and what the cache holds about an
  operator or a path with such a character is no longer worked out again on
  every run. The cache starts anew, so the first run after upgrading works
  everything out again. [Issue 290](https://github.com/mrkkrp/tilia/issues/290).
* Where the plan gives no hash to check a dependency's tarball against, a
  tarball that is not gzip, or is cut short, leaves its package unread, so
  that a module needing its fixities is declined, rather than ending the
  run. [Issue 303](https://github.com/mrkkrp/tilia/issues/303).
* The body of a typed Template Haskell quote written over several lines, as
  in `[|| … ||]`, is indented under its opening bracket together with its
  closing one, as that of an untyped quote is, rather than coming out at
  the column of the brackets.
  [Issue 300](https://github.com/mrkkrp/tilia/issues/300).
* A name an as-pattern binds at the top level, as `plus` in
  `plus@_ = (+)`, is taken for the module's own, rather than a module using
  it being declined where an import could not be read; a name bound inside
  the function of a view pattern, on the other hand, is not.
  [Issue 301](https://github.com/mrkkrp/tilia/issues/301).
* A module that re-exports the members of a type without the type, such as
  the methods of a class named by an operator, is no longer taken to declare
  the type's fixity, and a name a decoded interface file re-exports takes
  the fixity it has in the namespace it is exported in, as a type or as a
  term, rather than in both.
  [Issue 302](https://github.com/mrkkrp/tilia/issues/302).
* A conditional that begins among the bindings of a `where` clause and goes
  on into the declarations after it stays whole where the bindings are
  indented with a tab, as it already did where they are indented with
  spaces, rather than coming out in two.
  [Issue 297](https://github.com/mrkkrp/tilia/issues/297).
* A line that holds nothing but a use of a function-like macro and is
  indented with a tab is taken to be at the column the tab moves it to, as
  GHC takes it, rather than at the count of characters before it. One at the
  column of the code under it is put back as written, rather than the module
  being declined where the use does not read as code, and one further in is
  read as code that carries on the line above, rather than put back at the
  level of the code under it.
  [PR 333](https://github.com/mrkkrp/tilia/pull/333).
* A sub-library is looked for in the build plan under `lib:` and its name,
  as the plan writes it, rather than as the main library, so a package
  whose only library is a named one no longer makes the first run under a
  plan `cabal` solved ask it to solve again, and a sub-library the plan
  leaves out no longer counts as planned where the main library is.
  [Issue 296](https://github.com/mrkkrp/tilia/issues/296).
* A source directory that two packages name through different paths, as
  `shared` in one and `../a/shared` in the other, is taken to be one
  directory, so `check` and `inplace` format each module in it once, rather
  than once under each path.
  [Issue 295](https://github.com/mrkkrp/tilia/issues/295).
* A module that no `.cabal` file above it names, as one in a directory
  outside every package that packages of the project share through `..`,
  takes what a package of the project that names it puts in force, rather
  than failing for want of a `.cabal` file, and the fixities it declares
  are known to the modules importing it, rather than their being declined.
  [Issue 295](https://github.com/mrkkrp/tilia/issues/295).
* A character literal holding a double quote, as in `'"'`, is read as one,
  rather than as the start of a string, so a `{-# LINE #-}` written in a
  string after it no longer has the module declined, and one written in the
  code after it is no longer missed.
  [Issue 292](https://github.com/mrkkrp/tilia/issues/292).
* A line that holds nothing but a use of a function-like macro is put back
  at the indentation of the code under it, as it is written, rather than a
  step further in wherever what is around it begins on a line above. One
  among the statements of a `do` block, or between the equations of a
  function, no longer comes out carrying on the line above it.
  [Issue 279](https://github.com/mrkkrp/tilia/issues/279).
* A file whose formatting throws an exception is reported as failing, with
  the exception, rather than the exception ending the run with "thread
  blocked indefinitely in an MVar operation" and leaving every other file
  unwritten and unreported.
  [Issue 291](https://github.com/mrkkrp/tilia/issues/291).
* A dependency whose source is still missing after a download is no
  longer downloaded again on every run, nor is `cabal` asked twice where it
  keeps sources, which formatting on save paid for each time: what a
  download does not bring in is remembered under the plan, as it already
  was where the plan did not cover a component.
  [Issue 289](https://github.com/mrkkrp/tilia/issues/289).
* An entry of the `packages` field of a `cabal.project` that begins with
  dots or a slash, as `../lib`, `/abs/lib` or `.hidden`, is looked for where
  it points, as `cabal` looks for it, rather than under the project with the
  dots and the slash taken off, where it was usually not found and passed
  over. [Issue 294](https://github.com/mrkkrp/tilia/issues/294).
* A string literal comes out as written wherever it is: its gaps line up
  under its opening quote in a pattern and in a `WARNING` or `DEPRECATED`
  message, as they already did in an expression, a rule's name is no longer
  escaped again, as `"f/∘"` was into `"f/\8728"`, and an `SCC` label
  written without quotes no longer gains them.
  [PR 344](https://github.com/mrkkrp/tilia/pull/344).
* Top-level splices written one after another stay together across a
  comment or a preprocessor line between them, as standalone `deriving`
  declarations already did, rather than having blank lines put between
  them. [PR 344](https://github.com/mrkkrp/tilia/pull/344).
* An interrupt arriving while a package or a file is read stops the run,
  rather than being taken for that read failing.
  [PR 344](https://github.com/mrkkrp/tilia/pull/344).
* A run reads the build plan once where nothing is missing, and the sources
  of each dependency and the `.cabal` file of each package of the project
  once rather than once per component.
  [PR 344](https://github.com/mrkkrp/tilia/pull/344).
* The release binaries are built with GHC 9.14.1 rather than 9.10.3, and
  so read `.cabal` files with Cabal-syntax 3.16: a package declaring
  `cabal-version: 3.14` or `3.16` can be formatted, where the binaries said
  its `.cabal` file did not parse.

## Tilia 0.1.2.0

* A comment written beside the `..` of an item of an import or export list,
  as in `T ({- c -} ..)`, stays inside its parentheses rather than moving to
  the item after it. [Issue 254](https://github.com/mrkkrp/tilia/issues/254).
* A fixity declaration written without a precedence, such as
  ``infixr `cons` ``, comes out without one rather than with the `9` it
  defaults to. [Issue 267](https://github.com/mrkkrp/tilia/issues/267).
* The `where` of a module header written once after the `#endif` of a
  conditional around the export list stays there rather than being copied
  into each branch, and comments written right before or after it stay on
  their side of it. [Issue 266](https://github.com/mrkkrp/tilia/issues/266).
* Code that one branch of a conditional puts a prefix in front of, such as
  `again $`, comes out once after the `#endif` rather than in each branch
  where it is written at the column of the prefix.
  [Issue 263](https://github.com/mrkkrp/tilia/issues/263).
* A comment carrying on the remark that ends the line above stays with that
  remark where it is lined up under it and the code under the comment begins
  at the same column, or where it is indented further than the remark,
  rather than going with the code that follows.
  [Issue 255](https://github.com/mrkkrp/tilia/issues/255).
* With `UnboxedTuples` on, as with `UnboxedSums` already, an operator that
  begins with `#` keeps the space after an opening parenthesis, as in
  `( #<| )` and the section `( #<| x)`, and `#` alone the space before a
  closing one, as in `(x # )`, rather than coming out as `(#` or `#)`, which
  do not parse. [Issue 273](https://github.com/mrkkrp/tilia/issues/273).
* A comment under a quasi-quotation written over several lines stays out of
  it, rather than being moved before its `|]`, which changed what was
  quoted. [Issue 274](https://github.com/mrkkrp/tilia/issues/274).
* A module that begins with a byte order mark, which GHC skips, is
  formatted and keeps the mark, and fixities are taken from a project module
  that begins with one, rather than its not parsing.
  [Issue 275](https://github.com/mrkkrp/tilia/issues/275).
* `Tilia.Editor.compiledSession` builds a session from what a program that
  has compiled the project already knows, such as a language server, so that
  formatting needs neither Cabal nor the sources of dependencies.
  [Issue 234](https://github.com/mrkkrp/tilia/issues/234).

## Tilia 0.1.1.0

* Keep code that every configuration prints alike out of the branches of a
  conditional in many more cases rather than copying it into each of them.
  [PR 229](https://github.com/mrkkrp/tilia/pull/229).
* Lay a construct holding a quasi-quotation written over several lines out
  as if the quasi-quotation were written on one line, since its lines are
  reproduced. It stays on the line of what comes before it, and what follows
  its `|]` stays on the line of the `|]`, rather than going on lines of
  their own. [Issue 208](https://github.com/mrkkrp/tilia/issues/208).
* A run whose build plan is up to date and whose cache is filled starts no
  other program: where `cabal` keeps the sources it downloads is remembered,
  the compiler's version is taken from the plan, and a plan that solving
  left as it was is not solved again on every run after a `.cabal` file is
  saved. [PR 236](https://github.com/mrkkrp/tilia/pull/236).
* The fixity a module declares for an operator it does not export is no
  longer taken to be that operator's fixity in the modules importing it,
  whether the module is read from its source or from its interface file. A
  module that brings in an operator of the same name from elsewhere is no
  longer declined over the two disagreeing.
  [Issue 238](https://github.com/mrkkrp/tilia/issues/238).
* A record field whose first line is written in each branch of a
  conditional, with a body the branches share after the `#endif`, comes out
  as written, rather than as its name, a `=` on a line of its own and a
  second conditional, which formatting again indented further. Where several
  such fields follow one another, the body of the first is no longer copied
  into each branch. [Issue 240](https://github.com/mrkkrp/tilia/issues/240).
* The `VERSION_` and `MIN_VERSION_` macros of a package the build plan does
  not hold are taken to be undefined, as Cabal defines them only for the
  dependencies of a component. A module is no longer declined over an
  operator that only a branch behind `#ifdef VERSION_x` imports where the
  plan has no `x`. [Issue 241](https://github.com/mrkkrp/tilia/issues/241).
* A pattern written over several lines keeps `<-` and the expression after
  it on its last line where they were written there, in a pattern guard as
  in a `do` block, and the closing bracket of a list or tuple pattern in a
  pattern guard comes out under the opening one, rather than a step further
  in, which only a layout block needs.
  [Issue 210](https://github.com/mrkkrp/tilia/issues/210).
* The `of` of a `case` written on a line of its own after a conditional
  comes out indented past the `case`, as written, rather than at the column
  of the statement, where it began a new one and the module no longer
  parsed. [Issue 237](https://github.com/mrkkrp/tilia/issues/237).
* `--check-ast` no longer reports a comment as lost where it carries on the
  trailing comment of the last import and sorting moves that import, with
  the comment, above another.
  [Issue 252](https://github.com/mrkkrp/tilia/issues/252).
* A type or class imported with `(..)` from a module that exports it without
  its members, such as `ParseError (..)` from `Text.Parsec`, is no longer
  taken to bring in every operator that module exports. A module that brings
  in an operator of the same name from elsewhere is no longer declined over
  the two disagreeing. [Issue 253](https://github.com/mrkkrp/tilia/issues/253).
* A `case` whose head is written whole in each branch of a conditional comes
  out as written, rather than with `case` once above the conditional and the
  scrutinee at the column of the statement, where it began a new one and the
  module no longer parsed. An `of` written after the `#endif` goes at the
  end of each branch, and a comment written after `of` stays after it rather
  than in front of it. [Issue 250](https://github.com/mrkkrp/tilia/issues/250).
* A `let` whose `in` ends its line, followed by another `let` on the next
  line, is kept as one of a chain however many lines its bindings take. One
  written in each branch of a conditional stays there as written, rather
  than coming out with its `let` and `in` outside the conditional and, once
  formatted again, as an `in let` on one line.
  [Issue 251](https://github.com/mrkkrp/tilia/issues/251).

## Tilia 0.1.0.0

### Conditional compilation

Modules that use the C preprocessor are formatted in one pass, with their
comments where they were written, in all the cases found so far. Where the
configurations of a module differ in only a part of something, such as an
argument, an item of a list, the start of a statement, the Haddock of a
constructor or an overlap pragma in an instance head, the conditional stays
around that part in many more cases, rather than the code around it being
copied into each branch, and conditionals stay nested and whole as they were
written. A comment that comes out right above a directive goes to the
margin, and the empty lines around a conditional and at the ends of its
branches follow what the configurations print.

Directives come out as written: one continued with a backslash, a `#define`
continued over several lines and the directive under it, the text after
`#else` and `#endif`, and an alternative that holds nothing but `#error`. A
line that holds nothing but a use of a function-like macro the module
defines is put back as written rather than read as code. Configurations that
no definition of the macros gives are no longer tried, so a module is no
longer declined over one of them, and what only other compilers see, behind
`__MHS__` or `__HUGS__`, is not taken to be in scope. A module with many
conditionals is formatted a fragment at a time, so few run out of
configurations, and the fixities in a module whose branches do not parse
together are worked out from each configuration.

Issues and pull requests: [Issue 6], [Issue 7], [PR 34], [PR 39], [PR 44],
[PR 54], [PR 56], [PR 57], [Issue 60], [Issue 61], [Issue 64], [Issue 65],
[Issue 66], [Issue 68], [Issue 69], [Issue 70], [Issue 82], [PR 111],
[Issue 113], [Issue 117], [Issue 118], [Issue 133], [Issue 134],
[Issue 153], [Issue 154], [Issue 155], [Issue 167], [Issue 172],
[Issue 178], [Issue 179], [Issue 197].

### Operators and fixities

Tilia finds the fixity of an operator more often and more precisely. It
settles each name a module re-exports on its own, takes the names an import
hides into account in a whole-module re-export, and no longer blames an
operator on a module it cannot read where the operator cannot have come from
there: a name the module defines, one a local binding captures, which takes
the fixity its binding group declares, or one an import that can be read
brings in, among them the constructors, fields and methods of `T(..)` and
every name an interface file refers to by key. The fixities of the packages
that come with the compiler are read from its interface files rather than
from a table written for one version of it, and `:` has its fixity wherever
it is written. Where Tilia declines a file over an operator its imports
bring in with different fixities, it names each fixity and the imports that
bring it.

Issues and pull requests: [PR 52], [Issue 67], [Issue 84], [Issue 86],
[PR 98], [PR 101].

### Comments

Comments stay with the code they were written beside in many more places:
under the last line of a construct and lined up with it, after an operator
that ends a line or between that operator and its operand, between a name
and its `=`, above the `in` of a `let`, around the guards of a `case`,
between a Haddock and what it documents, inside empty brackets and before
closing ones, at the end of a line with comments lined up under it, under a
remark they carry on, and after the last declaration of a Backpack
signature, where they used to be dropped.

A section heading keeps the comments written right against it. An empty line
written under a comment is kept, none is put around a comment where the
author wrote none, and a comment is held off a Haddock only where it would
otherwise run into it. Block comments are spaced against brackets and commas
as written. A Haddock written with no space after its trigger, or holding
nothing but a space, is formatted.

Issues and pull requests: [Issue 62], [Issue 63], [PR 79], [Issue 94],
[Issue 119], [Issue 120], [Issue 121], [Issue 122], [Issue 132],
[Issue 137], [Issue 147], [Issue 148], [Issue 151], [Issue 152],
[Issue 158], [Issue 159], [Issue 180], [Issue 182], [Issue 183],
[Issue 184], [Issue 185], [PR 201], [Issue 205], [Issue 206], [Issue 217].

### Layout

More of the layout an author chose is kept where it reads well. A `do`
block, a `case` or a lambda stays on the line of the `$`, `=` or `->` it was
written after; a chain of `let … in`, each on a line of its own, comes out
as a column; the names of a signature for several names stay on one line
where they were written on one; and a module header with a `DEPRECATED` or
`WARNING` pragma on a line of its own keeps its lines. `OPTIONS_GHC` pragmas
keep the order they were written in, which is the order GHC reads their
flags in, and a `DEPRECATED` or `WARNING` pragma that names nothing is no
longer dropped.

Issues and pull requests: [Issue 96], [Issue 136], [Issue 181], [Issue 198],
[Issue 209], [Issue 211], [Issue 215], [Issue 218].

### Imports

An import list written on one line stays on one line. Imports of one module
are merged only where together they cannot bring in more than they did
apart: imports hiding different names stay apart, and so does a name in a
`hiding` list from the same name with its own parentheses. Sorting the
imports leaves the empty line under the module header and a heading comment
above the imports where they are, and it respects conditionals: an import
whose clause is behind one sorts among the others, the names on either side
of one in an import list sort apart, and `#define` lines no longer keep
apart what is written either side of them.

Issues and pull requests: [PR 55], [PR 73], [PR 76], [Issue 92],
[Issue 135], [Issue 150].

### Projects and tools

Tilia reads a Cabal package the way `cabal` builds it: a component that
names no `default-language` is Haskell98, the older `extensions` field
counts, and so do extensions behind a condition the build meets. A package
with a component named `all` gets a build plan. `.tiliaignore` files are
read the way Git reads `.gitignore` files, with globs, negation, and a file
in any directory. `tilia for-editor FILE`, and `Tilia.Editor` for programs
that use Tilia as a library, are there for editor integrations.

Issues and pull requests: [PR 41], [PR 51], [Issue 131], [Issue 156],
[Issue 186], [PR 200].

### Performance

Tilia is faster throughout. A run works out what a module offers once,
however many threads ask for it, parses each module that operators are
looked up in and reads each source tarball once, keeps summaries of the
project's own modules between runs, and reads what interface files refer to
by key rather than run `ghc --show-iface`. A run that starts with nothing
cached is up to three times as fast and needs half the memory.

Formatting walks a module's syntax tree once, reads its comments off the
tokens, places comments in time that does not grow with the square of the
size of a module, and works out the configurations of conditionals that do
not depend on one another on several cores. A module with thousands of
comments formats twice as fast, one whose conditionals reach from above the
declarations into them in less than half the time, and one with many
conditionals up to twice as fast where cores are free. `tilia check` diffs
faster.

Issues and pull requests: [PR 37], [PR 46], [PR 47], [PR 48], [PR 49],
[PR 50], [PR 130], [PR 201], [PR 202], [PR 203], [PR 228].

[Issue 6]: https://github.com/mrkkrp/tilia/issues/6
[Issue 7]: https://github.com/mrkkrp/tilia/issues/7
[PR 34]: https://github.com/mrkkrp/tilia/pull/34
[PR 37]: https://github.com/mrkkrp/tilia/pull/37
[PR 39]: https://github.com/mrkkrp/tilia/pull/39
[PR 41]: https://github.com/mrkkrp/tilia/pull/41
[PR 44]: https://github.com/mrkkrp/tilia/pull/44
[PR 46]: https://github.com/mrkkrp/tilia/pull/46
[PR 47]: https://github.com/mrkkrp/tilia/pull/47
[PR 48]: https://github.com/mrkkrp/tilia/pull/48
[PR 49]: https://github.com/mrkkrp/tilia/pull/49
[PR 50]: https://github.com/mrkkrp/tilia/pull/50
[PR 51]: https://github.com/mrkkrp/tilia/pull/51
[PR 52]: https://github.com/mrkkrp/tilia/pull/52
[PR 54]: https://github.com/mrkkrp/tilia/pull/54
[PR 55]: https://github.com/mrkkrp/tilia/pull/55
[PR 56]: https://github.com/mrkkrp/tilia/pull/56
[PR 57]: https://github.com/mrkkrp/tilia/pull/57
[Issue 60]: https://github.com/mrkkrp/tilia/issues/60
[Issue 61]: https://github.com/mrkkrp/tilia/issues/61
[Issue 62]: https://github.com/mrkkrp/tilia/issues/62
[Issue 63]: https://github.com/mrkkrp/tilia/issues/63
[Issue 64]: https://github.com/mrkkrp/tilia/issues/64
[Issue 65]: https://github.com/mrkkrp/tilia/issues/65
[Issue 66]: https://github.com/mrkkrp/tilia/issues/66
[Issue 67]: https://github.com/mrkkrp/tilia/issues/67
[Issue 68]: https://github.com/mrkkrp/tilia/issues/68
[Issue 69]: https://github.com/mrkkrp/tilia/issues/69
[Issue 70]: https://github.com/mrkkrp/tilia/issues/70
[PR 73]: https://github.com/mrkkrp/tilia/pull/73
[PR 76]: https://github.com/mrkkrp/tilia/pull/76
[PR 79]: https://github.com/mrkkrp/tilia/pull/79
[Issue 82]: https://github.com/mrkkrp/tilia/issues/82
[Issue 84]: https://github.com/mrkkrp/tilia/issues/84
[Issue 86]: https://github.com/mrkkrp/tilia/issues/86
[Issue 92]: https://github.com/mrkkrp/tilia/issues/92
[Issue 94]: https://github.com/mrkkrp/tilia/issues/94
[Issue 96]: https://github.com/mrkkrp/tilia/issues/96
[PR 98]: https://github.com/mrkkrp/tilia/pull/98
[PR 101]: https://github.com/mrkkrp/tilia/pull/101
[PR 111]: https://github.com/mrkkrp/tilia/pull/111
[Issue 113]: https://github.com/mrkkrp/tilia/issues/113
[Issue 117]: https://github.com/mrkkrp/tilia/issues/117
[Issue 118]: https://github.com/mrkkrp/tilia/issues/118
[Issue 119]: https://github.com/mrkkrp/tilia/issues/119
[Issue 120]: https://github.com/mrkkrp/tilia/issues/120
[Issue 121]: https://github.com/mrkkrp/tilia/issues/121
[Issue 122]: https://github.com/mrkkrp/tilia/issues/122
[PR 130]: https://github.com/mrkkrp/tilia/pull/130
[Issue 131]: https://github.com/mrkkrp/tilia/issues/131
[Issue 132]: https://github.com/mrkkrp/tilia/issues/132
[Issue 133]: https://github.com/mrkkrp/tilia/issues/133
[Issue 134]: https://github.com/mrkkrp/tilia/issues/134
[Issue 135]: https://github.com/mrkkrp/tilia/issues/135
[Issue 136]: https://github.com/mrkkrp/tilia/issues/136
[Issue 137]: https://github.com/mrkkrp/tilia/issues/137
[Issue 147]: https://github.com/mrkkrp/tilia/issues/147
[Issue 148]: https://github.com/mrkkrp/tilia/issues/148
[Issue 150]: https://github.com/mrkkrp/tilia/issues/150
[Issue 151]: https://github.com/mrkkrp/tilia/issues/151
[Issue 152]: https://github.com/mrkkrp/tilia/issues/152
[Issue 153]: https://github.com/mrkkrp/tilia/issues/153
[Issue 154]: https://github.com/mrkkrp/tilia/issues/154
[Issue 155]: https://github.com/mrkkrp/tilia/issues/155
[Issue 156]: https://github.com/mrkkrp/tilia/issues/156
[Issue 158]: https://github.com/mrkkrp/tilia/issues/158
[Issue 159]: https://github.com/mrkkrp/tilia/issues/159
[Issue 167]: https://github.com/mrkkrp/tilia/issues/167
[Issue 172]: https://github.com/mrkkrp/tilia/issues/172
[Issue 178]: https://github.com/mrkkrp/tilia/issues/178
[Issue 179]: https://github.com/mrkkrp/tilia/issues/179
[Issue 180]: https://github.com/mrkkrp/tilia/issues/180
[Issue 181]: https://github.com/mrkkrp/tilia/issues/181
[Issue 182]: https://github.com/mrkkrp/tilia/issues/182
[Issue 183]: https://github.com/mrkkrp/tilia/issues/183
[Issue 184]: https://github.com/mrkkrp/tilia/issues/184
[Issue 185]: https://github.com/mrkkrp/tilia/issues/185
[Issue 186]: https://github.com/mrkkrp/tilia/issues/186
[Issue 197]: https://github.com/mrkkrp/tilia/issues/197
[Issue 198]: https://github.com/mrkkrp/tilia/issues/198
[PR 200]: https://github.com/mrkkrp/tilia/pull/200
[PR 201]: https://github.com/mrkkrp/tilia/pull/201
[PR 202]: https://github.com/mrkkrp/tilia/pull/202
[PR 203]: https://github.com/mrkkrp/tilia/pull/203
[Issue 205]: https://github.com/mrkkrp/tilia/issues/205
[Issue 206]: https://github.com/mrkkrp/tilia/issues/206
[Issue 209]: https://github.com/mrkkrp/tilia/issues/209
[Issue 211]: https://github.com/mrkkrp/tilia/issues/211
[Issue 215]: https://github.com/mrkkrp/tilia/issues/215
[Issue 217]: https://github.com/mrkkrp/tilia/issues/217
[Issue 218]: https://github.com/mrkkrp/tilia/issues/218
[PR 228]: https://github.com/mrkkrp/tilia/pull/228

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
