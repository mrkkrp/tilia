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

## Benchmarks

The benchmarks format a fixed sample of the Hackage corpus, check what most
of its modules are printed as the way `--check-ast` does, and format a few
modules made to stress one part of the formatter each. They also work out
fixities the ways a run does: they read every module of a dozen packages of
the corpus out of their tarballs, as a run with a cold cache does, read the
same out of a cache, as a warm run does, and decode the interfaces of a few
packages the compiler ships with. They take about 45 seconds:

```console
$ cabal bench
```

What each benchmark allocates and the instructions it retires are the same
from one run to the next to a hundredth of a percent, however busy the
machine is, so they are kept in `bench/bench.record` and checked like the
corpus is. The run fails where a benchmark allocates or retires 0.5% more
or less than the record says, or where all the benchmarks of a stage
together do 0.05% more or less. Update the record like this, and review the
change to it like any other:

```console
$ TILIA_BENCH_ACCEPT=1 cabal bench
```

What is allocated depends on the compiler, so the record is made with the
base compiler, and CI checks it there. The instructions depend on the
processor as well, so they are checked only on the processor the record
names, where Linux lets a process count them; elsewhere the run says so and
checks the rest. What decoding the interfaces costs depends on what they
hold, which can differ between builds of one compiler, so the record keeps a
digest of their names and sizes, and decoding is checked only where it
matches; the bytes alone differ between two builds in fingerprints that cost
nothing to decode. The benchmarks run with `-A1g -O1g -C1000`, in about 1.2
GiB, so that few collections fall inside one, and no major one or context
switch: the collector's own instructions depend on where in the work it
runs, which any change to what is allocated moves, and a major collection or
a context switch makes the counts depend on the rest of the heap and on the
clock. The benchmarks of one module or package run on their own with
`--match`, and updating the record then changes only their lines:

```console
$ cabal bench --benchmark-options='--match QuickCheck'
```

To compare two versions on another processor, save what one measures and
compare the other with it, which shows the instructions and the time of
all the benchmarks together and the benchmarks that moved most:

```console
$ cabal bench --benchmark-options='--save /tmp/before'
$ cabal bench --benchmark-options='--baseline /tmp/before'
```

Time is not recorded: on a quiet machine the time of all the benchmarks
together moves by about 1% from one run to the next, and the time of one
of them by up to 13%, even with `--runs 3`, which measures each three times
and takes the fastest. On a busy machine it moves by far more.

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
builds the release binaries, runs weeder, the self-format checks and the
benchmarks, and provides the default development shell. When it changes,
which happens when the oldest version is dropped, update `GHC_VERSION` in
`.github/workflows/release.yaml` to match, as well as the `restore-keys` of
the `format` job and the compiler and `restore-keys` of the `bench` job in
`ci.yaml`, and record the benchmarks again with the new base compiler.

## Switching `ghc-lib-parser`

To switch to another version of `ghc-lib-parser`:

1. Change the bounds on `ghc-lib-parser` in `tilia.cabal`, in the library,
   the test suite and the benchmarks, and bump `index-state` in `cabal.project`
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
7. Record the benchmarks again with `TILIA_BENCH_ACCEPT=1 cabal bench`,
   since parsing takes a share of what they cost.
8. Update `CHANGELOG.md`.
