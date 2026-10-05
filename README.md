# Tilia

* [Getting started](#getting-started)
* [Editor integration](#editor-integration)
* [Excluding files](#excluding-files)
* [Formatting operator chains](#formatting-operator-chains)
* [Formatting CPP](#formatting-cpp)
* [Comparison with other formatters](#comparison-with-other-formatters)
* [Suggested setup per use-case](#suggested-setup-per-use-case)
* [Contribution](#contribution)
* [License](#license)

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

The two most useful commands are `inplace` and `check`:

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
package in the tree that neither `packages` nor `optional-packages` names is
not part of the project and will not be visited. Within a package, only the
modules a component declares get formatted: its `exposed-modules`,
`other-modules`, `signatures`, and `main-is`, in every conditional branch,
found under its `hs-source-dirs` as `.hs`, `.hs-boot`, or `.hsig` files.

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

* `--check-ast` performs an AST-equivalence check.
* `--check-idempotence` performs an idempotence check.
* `--debug-fixity` prints information that is useful for debugging
  formatting of operator chains.
* `--build-plan PLAN` trusts a given build plan as up to date rather than
  having Cabal solve one, see [Haskell.nix](#haskellnix).
* `--no-cache` neither reads from nor writes to the cache.
* `--no-downloads` does not download sources that are missing, and a file
  whose operators come from a dependency that could not be read is then
  declined.
* `--must-not-decline` turns declined files into failures.

## Editor integration

Tilia offers a dedicated command for editor integrations:

```console
$ tilia for-editor FILE < buffer.hs
```

It reads a module from standard input and formats it as if it were the
contents of `FILE` (a path): the project, the extensions in force, and the
dependencies are found by walking upwards from `FILE` rather than from the
working directory. `FILE` need not exist, but it has to be under the
`hs-source-dirs` of a component. The formatted module goes to standard
output. A module that is declined or excluded (see below), is returned
unchanged, and a failure exits with a non-zero status and prints nothing to
standard output, so an editor keeps its buffer as it is. The module is
always formatted as a whole; there is no formatting of a selected range.

For example, with [conform.nvim](https://github.com/stevearc/conform.nvim):

```lua
require("conform").setup({
  formatters = {
    tilia = { command = "tilia", args = { "for-editor", "$FILENAME" }, stdin = true },
  },
  formatters_by_ft = { haskell = { "tilia" } },
})
```

A program that would rather call Tilia as a library, such as a language
server plugin, can use `editorSession` and `formatBuffer` from
`Tilia.Editor`, which is what `for-editor` runs.

## Excluding files

You can tell Tilia to skip certain files and/or directories by listing them
in a `.tiliaignore` file, which Tilia reads the way Git reads `.gitignore`:

```gitignore
# Fixtures compiled by a separate driver
tests/shouldwork/
tests/shouldfail/

# Generated modules wherever they are, except one
*Generated.hs
!/src/Keep/Generated.hs
```

A `.tiliaignore` can be in the project root or in any directory below it,
and its patterns are relative to that directory. Everything `.gitignore`
offers works the same: the globs `*`, `?`, `[…]`, and `**`, a `/` at the
start or in the middle to anchor a pattern to its directory, a `/` at the
end to match directories only, `!` to re-include what an earlier pattern
excluded, `#` for comments, and `\` to escape any of these. As with Git, a
pattern in a deeper file wins over one above it, and a file whose directory
is excluded cannot be re-included.

## Formatting operator chains

There is nothing you need to know about it or do to make it work. It will
just happen, no matter where your operators come from: Hackage, Nix, private
repos, or the modules of the project you are formatting.

## Formatting CPP

CPP is a first-class formattable object to Tilia. Any Haskell syntactically
enclosed in a conditional branch will format, and it does not even need to
be self-contained valid Haskell on its own, as long as every configuration
of the module is a valid Haskell module.

## Comparison with other formatters

### Ormolu

* Ormolu formats operator chains by consulting a hardcoded library of
  operator fixities which it builds partly by running a rudimentary
  analysis over some hand-picked packages and partly by consuming a Hoogle
  dump. That hardcoded fixity library is built during development and then
  bundled into the executable. The library is necessarily both incomplete
  and prone to going out of date. Furthermore, Ormolu does not
  automatically account for custom operators that occur in the code it is
  asked to format. For that you need to write `.ormolu` files in which you
  redeclare the fixities of your custom operators and any relevant
  re-exports. Tilia guarantees resolution of operator fixities
  automatically at all times.
* Ormolu's CPP support is rudimentary. It splits the input file into
  sections that must be parseable on their own, then preserves CPP
  conditional blocks verbatim. First, the requirement for the sections to
  be parseable on their own is only sometimes satisfied—CPP directives
  tend to fall at arbitrary points in the code, which makes Ormolu choke.
  Second, preserving CPP conditional blocks verbatim is not good enough.
  For example, if Ormolu re-indents the surrounding code and the CPP
  conditional block stays as it was, the result is broken code.
* Ormolu has a very different CLI focused on explicit file names, so that
  its users find themselves running invocations like
  `ormolu -i $(git ls-files '*.hs' '*.hs-boot')`. Tilia focuses on Cabal
  components, which is arguably better ergonomics.
* Ormolu supports magic comments `{- ORMOLU_DISABLE -}` and
  `{- ORMOLU_ENABLE -}` while Tilia has no equivalent to those. The
  comments were introduced to work around the weaknesses of Ormolu's CPP
  support as well as its inability to respect grouping of certain types of
  definitions that the users wanted to preserve. Tilia both respects
  grouping in more situations and has first-class support for CPP, so
  these comments are not needed.
* Ormolu can be asked to format regions in a file with `--start-line` and
  `--end-line` options. Tilia has no such functionality since it operates
  at a higher level (Cabal components or whole projects), which is aligned
  with the current trends in software development.
* Ormolu is self-contained and makes no assumption about tools on the
  system where it is run. Tilia needs Cabal: it shells out to it and may
  download packages, unless the build plan is given with `--build-plan`
  and downloads are ruled out with `--no-downloads`. Ormolu does none of
  this, which may be an advantage in some situations.

### Fourmolu

* Fourmolu is a configurable fork of Ormolu which shares the same
  architecture, strengths, and weaknesses.

## Suggested setup per use-case

### Local development

Have `cabal` and the compiler your project is built with on `PATH`, as you
would to build it, and nothing else needs setting up. Tilia gets the build
plan and the sources of dependencies through Cabal as described above, and
maintains its cache in the user's cache directory: `~/.cache/tilia`, or
`%LOCALAPPDATA%\tilia` on Windows.

### CI with Cabal

On GitHub Actions, [setup-tilia](https://github.com/mrkkrp/setup-tilia)
installs Tilia and maintains its caches automatically, keyed on the version
of Tilia and on your `.cabal` and `cabal.project` files:

```yaml
- uses: haskell-actions/setup@v2
  with:
    ghc-version: '9.10.3'
- uses: actions/checkout@v7
- uses: mrkkrp/setup-tilia@v1
- run: tilia check
```

Nothing has to be built before the check.

### Haskell.nix

[haskell.nix](https://github.com/input-output-hk/haskell.nix) builds each
component with every dependency already installed and keeps the plan it
solved in `plan-nix`. `--build-plan` points Tilia at that plan, so Cabal is
not asked to solve one, and with `--no-downloads` nothing is fetched either:
only the interfaces of what is installed are read. Cabal is then not run at
all and need not be installed.

This makes a formatting check that runs as part of the build, before a
component is compiled, with no development shell to set up for it. Each
component is built from the whole package source, so each checks the
component it builds:

```nix
project = pkgs.haskell-nix.cabalProject {
  # ...
  modules = [{
    packages.my-package.components = {
      library.preBuild = tiliaCheck "lib:my-package";
      exes.my-exe.preBuild = tiliaCheck "exe:my-exe";
    };
  }];
};
tiliaCheck = target: ''
  ${tilia.packages.${system}.default}/bin/tilia check ${target} \
    --build-plan ${project.plan-nix}/plan.json \
    --no-cache \
    --no-downloads \
    --must-not-decline
'';
```

Here `tilia` is this repository as a flake input. There is nowhere to keep a
cache between Nix builds, hence `--no-cache`, so every check reads the
interfaces it needs again, which has been optimized to perform nearly as
fast as a cached run outside of Nix (a fraction of a second on a project the
size of Tilia).

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project][issue-tracker].

Pull requests are also welcome, see [HACKING.md](./HACKING.md).

[issue-tracker]: https://github.com/mrkkrp/tilia/issues

## License

Copyright © 2026–present Mark Karpov

Distributed under the BSD 3-clause license.
