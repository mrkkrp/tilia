# Hacking

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

## Adding a GHC version

Tilia is built and tested with every compiler in the `compilers` list in
`flake.nix`. To add a major version of GHC:

1. Add it to `compilers` in `flake.nix`, where the list goes from the oldest
   version to the newest. If `haskell.nix` does not know the compiler yet,
   update it first with `nix flake update haskellNix`. This also gives the
   compiler a development shell, such as `nix develop .#ghc9161`.
2. Add it to the `build` matrix in `.github/workflows/ci.yaml` and number
   the shards again, so that with `n` entries each takes its own `i/n`. The
   test groups named in `compilerBound` in `tests/Main.hs` run on every
   entry, and the rest of the suite is split between them.
3. Add it to `tested-with` in `tilia.cabal`, and widen the bounds for the
   new versions of boot libraries. Bump `index-state` in `cabal.project` if
   the plan needs newer releases from Hackage.
4. Generate the table of names that the series writes into interface files
   as keys, with the new compiler:

   ```console
   $ ./generate-known-keys.py --ghc /path/to/ghc-9.16.1/bin/ghc
   ```

   This writes `src/Tilia/Fixity/KnownKeys/GhcNNN.hs`, which goes into
   `exposed-modules` in `tilia.cabal`.
5. Teach `Tilia.Fixity.HiFile` to read the interface files of the series: a
   new `Series` constructor, its version in `hiFile`, its table in
   `keyedNames`, its `primops` in `primopFixities`, and whatever the series
   changed in the layout that `hiFile`, `payload`, `pointer`, and `fixity`
   read. GHC's `GHC.Iface.Binary` and `GHC.Unit.Module.ModIface`, compared
   between the two series, show what changed. Tilia passes interface it
   cannot decode to `ghc --show-iface`, which works but costs a process per
   interface. The tests fail until every interface of the project's
   dependencies decodes and agrees with `ghc --show-iface` where the two are
   compared:

   ```console
   $ nix develop .#ghc9161 -c cabal test --test-options='--match "Tilia.Fixity"'
   ```

6. Update `CHANGELOG.md`.

The first entry of `compilers` in `flake.nix` is the base compiler: it
builds the release binaries, runs weeder and the self-format checks, and
provides the default development shell. When it changes, which happens when
the oldest version is dropped, update `GHC_VERSION` in
`.github/workflows/release.yaml` to match, as well as the `restore-keys` of
the `format` job in `ci.yaml`, which fall back on the base compiler's
caches.

## Switching `ghc-lib-parser`

To switch to another version of `ghc-lib-parser`:

1. Change the bounds on `ghc-lib-parser` in `tilia.cabal`, both in the
   library and in the test suite, and bump `index-state` in `cabal.project`
   past its release. If `haskell.nix`'s index of Hackage does not reach that
   far, update the `haskellNix` input as well.
2. Fix what the new API breaks. Warnings are errors in this project, so a
   constructor that the new syntax tree adds and the printer does not handle
   shows up as an incomplete match. Add new syntax examples in
   `corpora/vendored/`.
3. Point `ghcTestSuite` in `tests/Tilia/Corpus.hs` at the test suite of the
   GHC release the parser comes from: its `corpusName` and the
   `ghc-X.Y.Z-release` tag it is fetched from. Then revise `ghcUnreadable`,
   the files GHC's own parser rejects, and `ghcDeclined`, the files Tilia is
   right to refuse, against that test suite.
4. Regenerate `tests/Tilia/BootFixities.hs`, the fixities the tests take the
   boot packages to export, with the compiler of the same release, after
   checking that `PACKAGES` in the script still names its boot packages:

   ```console
   $ ./generate-boot-fixities.py --ghc /path/to/ghc-9.16.1/bin/ghc
   ```

5. Record the Hackage corpus again with `TILIA_CORPUS_ACCEPT=1 cabal test`
   and review every change to `hackage.manifest` and `hackage.report`: a
   module that changes its outcome or its digest is now parsed differently.
6. The cache keeps summaries of local modules by the version of
   `ghc-lib-parser` that read them, but what it learned about dependencies
   is kept regardless of it. If the new parser reads the sources of
   dependencies differently, bump `formatVersion` in `Tilia.Fixity.Cache`.
7. Update `CHANGELOG.md`.
