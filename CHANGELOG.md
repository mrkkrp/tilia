## Unreleased

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
