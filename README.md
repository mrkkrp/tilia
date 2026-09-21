# Tilia

Tilia is a formatter for Haskell source code. Its primary design choices
are:

* Use `ghc-lib-parser` for parsing, thus achieving correct parsing at all
  times.
* Let single vs multiline layout be influenced by the input.
* Admit no configuration.
* Ensure high-quality formatting of comments.
* Provide first-class support for CPP.
* Guarantee inference of operator fixity with absolute precision at all
  times.

*If you are curious how Tilia works, see this [blog post][blog-post].*

[blog-post]: https://markkarpov.com/post/announcing-tilia

## Getting started

The two most useful (and only!) commands are `inplace` and `check`:

```console
$ tilia inplace [COMPONENT] # format all files of COMPONENT in place
$ tilia check   [COMPONENT] # check that all files of COMPONENT are formatted
```

`COMPONENT` may be omitted and in that case it defaults to `all`. To be
precise, the kind of component we are talking about is exactly Cabal's
notion of component: libraries, executables, test suites, and benchmarks.
For example, in the case of Tilia itself the valid choices are:

* `all`
* `tilia`, the package, which means every component of it
* `lib:tilia` or `tilia:lib:tilia`
* `exe:tilia` or `tilia:exe:tilia`
* `test:tests` or `tilia:test:tests`, or just `tests`

It may be surprising that we talk about components rather than individual
files. Well, formatting a Haskell module, fortunately or unfortunately,
depends on much more than the input text. It depends on things like
`default-extensions`, `default-language`, and, most importantly, the actual
dependencies, because that's where the fixities of the operators you use
come from. What all these things have in common is that they are properties
of the respective Cabal components your modules belong to. Therefore, it
makes sense to consider those components the unit of formatting rather than
individual files.

Tilia respects Cabal projects as defined by `cabal.project` files. It finds
the project by starting at the working directory and walking upwards for a
`cabal.project` or a `.cabal` file. A `cabal.project` anywhere above wins
over a `.cabal` file that is nearer, so a package inside a multi-package
repository resolves to the repository. It is worth pointing out that a
package in the tree that neither `packages` nor `optional-packages` names
is not part of the project and will not be visited. Within a package, a
component's `hs-source-dirs` say which files belong to it, and every `.hs`,
`.hs-boot`, and `.hsig` under them gets formatted unless it is excluded,
as described below.

If there is no build plan yet, or it is older than the `.cabal` and
`cabal.project` files, or it says nothing about a component you asked for,
Tilia has Cabal solve it with `cabal build all --dry-run`. If the plan is
fine but some dependencies have been neither downloaded nor built, it
fetches them with `cabal build all --only-download`. These commands do not
build anything, and both are one-time costs, since Cabal's package cache is
shared between projects. So do not worry if the first run in a project
prints a few lines from Cabal before Tilia starts formatting. Later runs
check the plan with a read and a `stat` per package.

Both of those calls also pass `--enable-tests` and `--enable-benchmarks`,
because test suites and benchmarks are components Tilia formats, but they
are often not enabled by default and that would be confusing. Where a
project will not solve with those flags, Tilia settles for what Cabal builds
by default, so you get a narrower plan rather than none.

Finally, here are some other flags that may be of interest:

* `--check-ast` performs an AST-equivalence check;
* `--check-idempotence` performs an idempotence check;
* `--debug-fixity` prints information that is useful for debugging
  formatting of operator chains.

## Excluding files

You can tell Tilia to skip certain files and/or directories. To do so, list
their paths in a `.tiliaignore` file at the project root:

```text
# Fixtures compiled by a separate driver
tests/shouldwork/
tests/shouldfail/
```

Entries are literal file or directory paths relative to the project root,
and a directory excludes everything below it. Blank lines, surrounding
whitespace, and lines beginning with `#` are ignored. Wildcards and
re-inclusion patterns are not supported.

## Formatting operator chains

There is nothing you need to know about it or do to make it work. It will
just happen, no matter where your operators come from: Hackage, Nix, private
repos, or the modules of the project you are formatting.

## Formatting CPP

CPP is a first-class formattable object to Tilia. Any Haskell syntactically
enclosed in a conditional branch will format, and it does not even need to
be self-contained valid Haskell on its own, as long as every configuration
of the module is a valid Haskell module.

## Development

Enter the development shell by either running `direnv allow` or `nix
develop`. Once in the shell, the development is ordinary Cabal:

```console
$ cabal build
$ cabal test
```

All tests are in one test suite and there are a fair number of them. On my
machine the full test suite passes in 140 seconds, but it may be different
for you, so isolating a subset of the test suite may be helpful:

```console
$ cabal test --test-options='--match "Tilia.Fixity"'
```

The test suite will perform downloads the first time you run it and so it
will be a bit slower on that run. It needs various corpora, such as Hackage
packages and GHC's own test suite, which are not checked into this
repository.

The Hackage corpus is exercised in order to ensure that every module
formats, that its AST is preserved, and that formatting it is idempotent.
The results are recorded in `corpora/hackage/hackage.manifest`. Next to it,
`hackage.report` explains the failing cases. The manifest and the report can
be updated like this:

```console
$ TILIA_CORPUS_ACCEPT=1 cabal test
```

Finally, Tilia formats itself, so make sure to run this command before you
open a PR:

```console
$ nix run .#format
```

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project][issue-tracker].

Pull requests are also welcome.

[issue-tracker]: https://github.com/mrkkrp/tilia/issues

## License

Copyright © 2026–present Mark Karpov

Distributed under the BSD 3-clause license.
