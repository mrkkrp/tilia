{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The whole fixity pipeline, run against this project's own dependencies.
--
-- These tests read the real build plan, the real package cache and real
-- Hackage sources. That is the point: every other test in the suite works
-- on constructed inputs, and constructed inputs are exactly what a pipeline
-- that talks to the outside world will not fail on.
--
-- Running the test suite implies the project was built, so the plan and the
-- sources are there. Where they are not — a sandboxed build with no package
-- cache — each test says so and is marked pending rather than failing.
module Tilia.Fixity.PlanSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import Data.Choice (pattern Is)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.List qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (dropExtension, takeBaseName, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Fixity
import Tilia.Fixity.PackageDb (compilerIdentity)
import Tilia.Fixity.Plan
import Tilia.Parser
import Tilia.Process (readProgramOutput)
import Tilia.WithProjectPlan (withProjectPlan)

spec :: Spec
spec = do
  preparation
  tokens
  reexports
  hscModules
  generatedModuleSpec
  gitDependencies
  repositories
  packageCache
  withProjectPlan withPlan

-- | What a cached failure is filed under.
--
-- A failure to read a module leans on the plan and on the compiler this run
-- can ask, so both have to be in the token. Were the environment left out,
-- a shell that cannot see a package would hand its "could not be read" to
-- one that can.
tokens :: Spec
tokens = describe "the token a plan is cached under" $ do
  it "differs between environments over the same plan" $
    tokenForEnvAndBuildPlan "/one/bin/ghc-pkg" onePackage
      `shouldNotBe` tokenForEnvAndBuildPlan "/another/bin/ghc-pkg" onePackage

  it "differs between plans in the same environment" $
    tokenForEnvAndBuildPlan here onePackage
      `shouldNotBe` tokenForEnvAndBuildPlan here noPackages

  it "is the same twice over for the same plan and environment" $
    tokenForEnvAndBuildPlan here onePackage
      `shouldBe` tokenForEnvAndBuildPlan here onePackage

  it "asks the environment it is actually going to read in" $ do
    asked <- tokenForBuildPlan onePackage
    environment <- compilerIdentity
    asked `shouldBe` tokenForEnvAndBuildPlan environment onePackage
  where
    here = "/somewhere/bin/ghc-pkg"
    noPackages = BuildPlan{bpCompiler = "ghc-9.10.3", bpPackages = []}
    onePackage =
      BuildPlan
        { bpCompiler = "ghc-9.10.3",
          bpPackages =
            [ PlanPackage
                { ppName = "containers",
                  ppVersion = "0.7",
                  ppSource = PreExisting,
                  ppComponents = []
                }
            ]
        }

-- | What a run does before it trusts the plan.
--
-- These need no plan of their own and no @cabal@: the point is the order of
-- the steps, so the steps are recorded rather than taken.
preparation :: Spec
preparation = describe "preparing a project" $ do
  it "solves and then fetches, in the one run" $
    withTempProject Nothing $ \dir -> do
      steps <- newIORef []
      let cabal args = do
            record steps args
            when (args == solving) (writePlan dir wantingATarball)
            pure (Right ())
      checkReadiness [] dir `shouldReturn` PlanMissing
      prepareWith cabal undiscoveredFutility [] dir PlanMissing `shouldReturn` Right ()
      readIORef steps
        `shouldReturn` [solving, fetching]

  it "fetches without solving when the plan is already good" $
    withTempProject (Just wantingATarball) $ \dir -> do
      steps <- newIORef []
      readiness <- checkReadiness [] dir
      readiness `shouldBe` SourcesMissing ["tilia-phantom"]
      prepareWith (obliging steps) undiscoveredFutility [] dir readiness `shouldReturn` Right ()
      readIORef steps `shouldReturn` [fetching]

  it "runs nothing at all when nothing is missing" $ do
    steps <- newIORef []
    prepareWith (obliging steps) undiscoveredFutility [] "." Ready `shouldReturn` Right ()
    readIORef steps `shouldReturn` []

  it "does not go on to fetch when the solve fails" $
    withTempProject Nothing $ \dir -> do
      steps <- newIORef []
      let cabal args = record steps args >> pure (Left "cabal said no")
      prepareWith cabal undiscoveredFutility [] dir PlanMissing `shouldReturn` Left "cabal said no"
      readIORef steps `shouldReturn` [solving, narrowSolve]

  it "asks about the test suites and the benchmarks, not the library alone" $
    withTempProject Nothing $ \dir -> do
      steps <- newIORef []
      let cabal args = do
            record steps args
            when (args == solving) (writePlan dir wantingATarball)
            pure (Right ())
      _ <- prepareWith cabal undiscoveredFutility [] dir PlanMissing
      asked <- readIORef steps
      asked `shouldSatisfy` all (\args -> wholeProject `Data.List.isSuffixOf` args)

  it "settles for what will solve when the whole project will not" $
    withTempProject Nothing $ \dir -> do
      steps <- newIORef []
      let cabal args = do
            record steps args
            if wholeProject `Data.List.isSuffixOf` args
              then pure (Left "a test suite will not solve")
              else do
                when (args == narrowSolve) (writePlan dir wantingATarball)
                pure (Right ())
      prepareWith cabal undiscoveredFutility [] dir PlanMissing `shouldReturn` Right ()
      readIORef steps
        `shouldReturn` [solving, narrowSolve, fetching, narrowFetch]

  describe "a plan narrower than the run" $ do
    it "notices a component the plan says nothing about" $
      withTempProject (Just twoComponents) $ \dir ->
        checkReadiness [component "test:tests"] dir
          `shouldReturn` PlanNarrow ["thing:test:tests"]

    it "names every one it is missing" $
      withTempProject (Just twoComponents) $ \dir ->
        checkReadiness [component "test:tests", component "bench:speed"] dir
          `shouldReturn` PlanNarrow ["thing:test:tests", "thing:bench:speed"]

    it "is content with the components the plan does cover" $
      withTempProject (Just twoComponents) $ \dir ->
        checkReadiness [component "lib", component "exe:thing"] dir
          `shouldReturn` Ready

    it "asks for nothing when the run asks about nothing" $
      withTempProject (Just twoComponents) $ \dir ->
        checkReadiness [] dir `shouldReturn` Ready

    it "solves again rather than trusting it" $
      withTempProject (Just twoComponents) $ \dir -> do
        steps <- newIORef []
        let cabal args = do
              record steps args
              when (args == solving) (writePlan dir twoComponents)
              pure (Right ())
        prepareWith cabal undiscoveredFutility [component "test:tests"] dir (PlanNarrow ["thing:test:tests"])
          `shouldReturn` Right ()
        readIORef steps `shouldReturn` [solving]

    it "counts the components of this very project as covered" $ do
      plan' <- readBuildPlan (planPathFor ".")
      case plan' of
        Left _ -> pendingWith "no build plan; run cabal build first"
        Right p ->
          checkReadiness (plannedComponents p) "."
            `shouldNotReturn` PlanNarrow []

    it "solves only the once when solving does not widen it" $
      withTempProject (Just twoComponents) $ \dir -> do
        steps <- newIORef []
        futile <- newIORef False
        let cabal args = do
              record steps args
              when (args == solving) (writePlan dir twoComponents)
              pure (Right ())
            futility =
              undiscoveredFutility
                { solveWasFutile = readIORef futile,
                  rememberFutileSolve = writeIORef futile True
                }
            narrow = PlanNarrow ["thing:test:tests"]
            once = prepareWith cabal futility [component "test:tests"] dir narrow
        once `shouldReturn` Right ()
        readIORef futile `shouldReturn` True
        once `shouldReturn` Right ()
        readIORef steps `shouldReturn` [solving]

    it "fetches what a plan it cannot widen is short of" $
      withTempProject (Just narrowAndWanting) $ \dir -> do
        steps <- newIORef []
        let cabal args = do
              record steps args
              when (args == solving) (writePlan dir narrowAndWanting)
              pure (Right ())
            narrow = PlanNarrow ["thing:test:tests"]
        prepareWith cabal undiscoveredFutility [component "test:tests"] dir narrow
          `shouldReturn` Right ()
        readIORef steps
          `shouldReturn` [solving, fetching]

    it "fetches it even once solving again has been given up on" $
      withTempProject (Just narrowAndWanting) $ \dir -> do
        steps <- newIORef []
        futile <- newIORef True
        let futility =
              undiscoveredFutility
                { solveWasFutile = readIORef futile,
                  rememberFutileSolve = writeIORef futile True
                }
            narrow = PlanNarrow ["thing:test:tests"]
        prepareWith (obliging steps) futility [component "test:tests"] dir narrow
          `shouldReturn` Right ()
        readIORef steps `shouldReturn` [fetching]

    it "does not ask again for what fetching did not bring in" $
      withTempProject (Just narrowAndWanting) $ \dir -> do
        steps <- newIORef []
        refused <- newIORef []
        let futility =
              undiscoveredFutility
                { solveWasFutile = pure True,
                  fetchWasFutileFor = readIORef refused,
                  rememberFutileFetch = writeIORef refused
                }
            narrow = PlanNarrow ["thing:test:tests"]
            again = prepareWith (obliging steps) futility [component "test:tests"] dir narrow
        again `shouldReturn` Right ()
        readIORef refused `shouldReturn` ["tilia-phantom"]
        again `shouldReturn` Right ()
        readIORef steps `shouldReturn` [fetching]

    it "goes on solving while solving still widens it" $
      withTempProject (Just twoComponents) $ \dir -> do
        steps <- newIORef []
        futile <- newIORef False
        let cabal args = do
              record steps args
              when (args == solving) (writePlan dir threeComponents)
              pure (Right ())
            futility =
              undiscoveredFutility
                { solveWasFutile = readIORef futile,
                  rememberFutileSolve = writeIORef futile True
                }
        prepareWith cabal futility [component "test:tests"] dir (PlanNarrow ["thing:test:tests"])
          `shouldReturn` Right ()
        readIORef futile `shouldReturn` False
        readIORef steps `shouldReturn` [solving]

  describe "a package the plan could not take apart" $ do
    it "counts the components it lists under one entry" $
      withTempProject (Just plannedWhole) $ \dir ->
        checkReadiness [component "lib", component "test:spec"] dir
          `shouldReturn` Ready

    it "still misses one that entry does not list" $
      withTempProject (Just plannedWhole) $ \dir ->
        checkReadiness [component "bench:speed"] dir
          `shouldReturn` PlanNarrow ["thing:bench:speed"]

    it "does not offer the Setup program as a component" $ do
      plan' <- withTempProject (Just plannedWhole) (readBuildPlan . planPathFor)
      fmap plannedComponents plan'
        `shouldBe` Right [component "lib", component "test:spec"]

-- | Chasing an operator a module passes on rather than declares.
--
-- No plan and no network here: the modules a name could have come from
-- answer out of a table written below, which is what makes it possible to
-- ask not merely whether an answer came back but which module it came from.
reexports :: Spec
reexports = describe "an operator a module passes on" $ do
  it "comes from the module the qualifier names" $
    chased "module M ((Disp.<+>)) where\nimport Control.Arrow (first)\nimport qualified Text.PrettyPrint as Disp\n"
      `shouldReturn` Just (Fixity LeftAssoc 6)

  it "comes from an import that brings it in, not one that hides it" $
    chased "module M ((<+>)) where\nimport Control.Arrow hiding ((<+>))\nimport Text.PrettyPrint\n"
      `shouldReturn` Just (Fixity LeftAssoc 6)

  it "comes from an import that brings it in, not one that never names it" $
    chased "module M ((<+>)) where\nimport Control.Arrow (first)\nimport Text.PrettyPrint\n"
      `shouldReturn` Just (Fixity LeftAssoc 6)

  it "does not come from a qualified import when it is written plainly" $
    chased "module M ((<+>)) where\nimport Control.Arrow\nimport qualified Text.PrettyPrint as Disp\n"
      `shouldReturn` Just (Fixity RightAssoc 5)

  it "is the module's own where the module declares it" $
    chased "module M ((<+>)) where\nimport Control.Arrow\ninfixr 3 <+>\n(<+>) :: Int -> Int -> Int\na <+> b = a + b\n"
      `shouldReturn` Just (Fixity RightAssoc 3)

  it "is not answered at all when the module it came from cannot be read" $
    chased "module M ((<+>)) where\nimport No.Such.Module\n"
      `shouldReturn` Nothing

  it "comes from a type handed on whole, which carries it" $
    chased "module M (Doc (..)) where\nimport Text.PrettyPrint\n"
      `shouldReturn` Just (Fixity LeftAssoc 6)

  it "does not come from a type handed on by an import that hides it" $
    chased "module M (Doc (..)) where\nimport Text.PrettyPrint hiding (Doc (..))\nimport Control.Arrow\n"
      `shouldReturn` Nothing

-- | What a module written for @hsc2hs@ amounts to, by either route to one.
--
-- Both fixtures below are what @hsc2hs@ takes and no compiler does, and
-- both write a fixity that the answer is expected to ignore. That is the
-- point: a module nothing can read is answered out of
-- 'Tilia.Fixity.ByHand.hscFixities' or not at all, and were either answer
-- arrived at by reading the file there would be no answer to give.
hscModules :: Spec
hscModules = describe "a module written for hsc2hs" $ do
  it "declares nothing, where it is one of the project's own" $
    withFakeProject [("src/Cursed.hsc", cursed)] $
      \rs -> do
        askFixities rs "Cursed" `shouldReturn` Just Map.empty
        askExportNames rs "Cursed" `shouldReturn` Just Set.empty
        askChildren rs "Cursed" `shouldReturn` Map.empty

  it "declares what the table says, where it comes out of a tarball"
    $ withFakeArchive
      [ ("Cursed.hsc", cursed),
        ("System/Posix/Signals.hsc", signals)
      ]
    $ \rs -> do
      askFixities rs "Cursed" `shouldReturn` Just Map.empty
      askExportNames rs "System.Posix.Signals"
        `shouldReturn` Just (Set.fromList [OpName "addSignal", OpName "deleteSignal"])

-- | Where a package fetched from a repository is looked for.
repositories :: Spec
repositories = describe "a package fetched from a repository" $ do
  it "finds one Hackage downloaded"
    $ withCache
      [("hackage.haskell.org", "thing", "1.0")]
      (fromRepository "{\"type\":\"secure-repo\",\"uri\":\"http://hackage.haskell.org/\"}")
    $ \found -> found `shouldSatisfy` isUnder "hackage.haskell.org"

  it "finds one a private repository downloaded, named after its host"
    $ withCache
      [("packages.example.com", "thing", "1.0")]
      (fromRepository "{\"type\":\"secure-repo\",\"uri\":\"https://packages.example.com/\"}")
    $ \found -> found `shouldSatisfy` isUnder "packages.example.com"

  it "finds one whose directory is not named after its host"
    $ withCache
      [("my-company", "thing", "1.0")]
      (fromRepository "{\"type\":\"secure-repo\",\"uri\":\"https://packages.example.com/\"}")
    $ \found -> found `shouldSatisfy` isUnder "my-company"

  it "leaves a file+noindex repository's tarballs where they are" $
    withSystemTempDirectory "tilia-noindex" $ \repo -> do
      T.writeFile (repo </> "thing-1.0.tar.gz") "not really a tarball"
      withCache
        []
        (fromRepository ("{\"type\":\"local-repo-no-index\",\"path\":\"" <> T.pack repo <> "\"}"))
        $ \found -> found `shouldBe` (repo </> "thing-1.0.tar.gz")

  it "says where its own repository would put one nothing has downloaded" $
    withCache [] (fromRepository "{\"type\":\"secure-repo\",\"uri\":\"https://packages.example.com/\"}") $
      \found -> found `shouldSatisfy` isUnder "packages.example.com"

  it "falls back on Hackage for a plan that names no repository at all" $
    withCache [("hackage.haskell.org", "thing", "1.0")] planWithoutARepository $
      \found -> found `shouldSatisfy` isUnder "hackage.haskell.org"

-- | Is the tarball under this repository's directory of the cache?
isUnder :: FilePath -> FilePath -> Bool
isUnder repo path = ("/" <> repo <> "/") `Data.List.isInfixOf` path

-- | Where the package cache is looked for.
--
-- @cabal@ answers this differently on each platform and has answered it two
-- ways on this one, so the rule is to look everywhere it could be and take
-- whichever place holds an index. These drive the search by moving the
-- directories it derives from, which on Unix are these two variables.
packageCache :: Spec
packageCache = describe "where the package cache is looked for" $ do
  -- Whatever cabal says it is. Checked against cabal rather than against a
  -- path spelled out here, because the whole point of asking is that this
  -- suite cannot know what the answer should be on somebody else's
  -- machine—and did not, on Windows.
  it "is the directory cabal reports" $ do
    said <- readProgramOutput "cabal" ["path", "--remote-repo-cache"]
    case said of
      Nothing -> pendingWith "no cabal on the path to ask"
      Just reported ->
        packageCacheRoot `shouldReturn` T.unpack (T.strip reported)

  describe "and where it is guessed, for a cabal too old to ask" $ do
    it "is what CABAL_DIR says, above all else" $
      withSystemTempDirectory "tilia-cabal-dir" $ \dir ->
        withEnvironment [("CABAL_DIR", dir)] $
          guessedPackageCacheRoot `shouldReturn` (dir </> "packages")

    it "is the XDG cache where the index is there" $
      withLayouts $ \xdg _ -> do
        withIndexIn xdg
        guessedPackageCacheRoot `shouldReturn` xdg

    it "is still the old directory where the index is there instead" $
      withLayouts $ \_ legacy -> do
        withIndexIn legacy
        guessedPackageCacheRoot `shouldReturn` legacy

    it "is the platform's own default where there is no index anywhere" $
      withLayouts $
        \xdg _ -> guessedPackageCacheRoot `shouldReturn` xdg

-- | Run something against a home and an XDG cache directory of its own,
-- handing it both of the places a cache could then be in.
withLayouts :: (FilePath -> FilePath -> Expectation) -> Expectation
withLayouts act =
  withSystemTempDirectory "tilia-home" $ \home ->
    withSystemTempDirectory "tilia-xdg" $ \cache ->
      withEnvironment [("HOME", home), ("XDG_CACHE_HOME", cache)] $
        bracket
          (lookupEnv "CABAL_DIR" <* unsetEnv "CABAL_DIR")
          (traverse_ (setEnv "CABAL_DIR"))
          (const (act (cache </> "cabal" </> "packages") (home </> ".cabal" </> "packages")))

-- | Put a Hackage index where a cache directory would have one.
withIndexIn :: FilePath -> IO ()
withIndexIn root = do
  createDirectoryIfMissing True (root </> "hackage.haskell.org")
  T.writeFile (root </> "hackage.haskell.org" </> "01-index.tar") ""

-- | Run something on where @plannedTarballs@ looked, against a package
-- cache holding the entries given.
withCache ::
  -- | Repository directory, package, version — one per cached tarball.
  [(FilePath, Text, Text)] ->
  -- | The plan to read it against.
  Text ->
  (FilePath -> Expectation) ->
  Expectation
withCache cached planText act =
  withSystemTempDirectory "tilia-cabal" $ \cabalDir -> do
    traverse_ (put cabalDir) cached
    createDirectoryIfMissing True (cabalDir </> "packages")
    withEnvironment [("CABAL_DIR", cabalDir)] $
      withSystemTempDirectory "tilia-repo-plan" $ \dir -> do
        createDirectoryIfMissing True (takeDirectory (planPathFor dir))
        T.writeFile (planPathFor dir) planText
        readBuildPlan (planPathFor dir) >>= \case
          Left why -> expectationFailure (T.unpack why)
          Right plan ->
            plannedTarballs plan >>= \case
              [(_, found)] -> act found
              other -> expectationFailure (show (fmap snd other))
  where
    put cabalDir (repo, held, version) = do
      let at =
            cabalDir
              </> "packages"
              </> repo
              </> T.unpack held
              </> T.unpack version
      createDirectoryIfMissing True at
      T.writeFile
        (at </> T.unpack (held <> "-" <> version <> ".tar.gz"))
        "not really a tarball"

-- | A plan naming one package fetched from the repository described.
fromRepository :: Text -> Text
fromRepository repo =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\
  \\"pkg-src\":{\"type\":\"repo-tar\",\"repo\":"
    <> repo
    <> "}}]}"

-- | The same, as an older @cabal@ wrote it: a tarball and no more.
planWithoutARepository :: Text
planWithoutARepository =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\
  \\"pkg-src\":{\"type\":\"repo-tar\"}}]}"

-- | A dependency that arrived as a @source-repository-package@.
--
-- @cabal@ clones one into the project's own @dist-newstyle@ and says
-- nothing in the plan about where, so the whole question is whether the
-- clone is found. Once it is, it is a directory of sources like any other
-- and nothing below here is new.
gitDependencies :: Spec
gitDependencies = describe "a dependency that arrived as a git checkout" $ do
  it "finds where cabal unpacked it" $
    withFakeCheckout [("thing-2a9f", "1.0")] $ \dir ->
      sourcesOf dir
        `shouldReturn` [CheckedOut (dir </> "dist-newstyle" </> "src" </> "thing-2a9f")]

  it "leaves one alone that cabal has not unpacked yet" $
    withFakeCheckout [] $
      \dir -> sourcesOf dir `shouldReturn` [SourceRepo]

  it "passes over the clone of a revision that has been moved on from" $
    withFakeCheckout [("thing-0000", "0.9"), ("thing-ffff", "1.0")] $ \dir ->
      sourcesOf dir
        `shouldReturn` [CheckedOut (dir </> "dist-newstyle" </> "src" </> "thing-ffff")]

  it "passes over a directory named for another package" $
    withFakeCheckout [("other-2a9f", "1.0")] $ \dir ->
      sourcesOf dir `shouldReturn` [SourceRepo]

  it "reads a module out of it, fixity and all" $
    withFakeCheckout [("thing-2a9f", "1.0")] $ \dir -> do
      plan <- readBuildPlan (planPathFor dir)
      case plan of
        Left why -> expectationFailure (T.unpack why)
        Right p -> do
          rs <- newResolver p
          askFixities rs "Private.Ops"
            >>= (`shouldBe` Just (Map.singleton (InTerms, OpName "<+>") (Fixity RightAssoc 3)))

-- | What the plan says each of its packages came from.
sourcesOf :: FilePath -> IO [PackageSource]
sourcesOf dir =
  readBuildPlan (planPathFor dir) >>= \case
    Left why -> error (T.unpack why)
    Right plan -> pure (fmap ppSource (bpPackages plan))

-- | A project whose one dependency is a @source-repository-package@, with
-- the clones given unpacked where @cabal@ unpacks them.
--
-- Each clone is a directory name and the version its @.cabal@ file claims,
-- because telling one clone from another is the whole of the work.
withFakeCheckout :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withFakeCheckout clones act =
  withSystemTempDirectory "tilia-checkout" $ \dir -> do
    createDirectoryIfMissing True (takeDirectory (planPathFor dir))
    T.writeFile (planPathFor dir) fromAGitRepository
    traverse_ (unpack dir) clones
    act dir
  where
    unpack dir (named, version) = do
      let at = dir </> "dist-newstyle" </> "src" </> named
          belongsTo = takeWhile (/= '-') named
      createDirectoryIfMissing True (at </> "src" </> "Private")
      T.writeFile (at </> belongsTo <> ".cabal") (describing (T.pack belongsTo) version)
      T.writeFile (at </> "src" </> "Private" </> "Ops.hs") privateOps
    describing named' version =
      T.unlines
        [ "cabal-version: 2.4",
          "name: " <> named',
          "version: " <> version,
          "library",
          "  exposed-modules: Private.Ops",
          "  hs-source-dirs: src",
          "  default-language: Haskell2010"
        ]

-- | A plan naming one package that came out of a git repository.
fromAGitRepository :: Text
fromAGitRepository =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\
  \\"pkg-src\":{\"type\":\"source-repo\",\
  \\"source-repo\":{\"type\":\"git\",\"location\":\"git://example/thing\"}}}]}"

-- | A module of that package, declaring something worth resolving.
privateOps :: Text
privateOps =
  T.unlines
    [ "module Private.Ops ((<+>)) where",
      "infixr 3 <+>",
      "(<+>) :: Int -> Int -> Int",
      "a <+> b = a + b"
    ]

-- | The modules @cabal@ writes, which no package carries a file for.
generatedModuleSpec :: Spec
generatedModuleSpec = describe "a module cabal generates" $ do
  it "declares nothing, rather than being one we could not read" $
    withFakeProject [("src/M.hs", "module M where\nimport Paths_fake\n")] $
      \rs -> askFixities rs "Paths_fake" `shouldReturn` Just Map.empty

  it "answers for the newer one cabal writes beside it" $
    withFakeProject [("src/M.hs", "module M where\n")] $
      \rs -> askFixities rs "PackageInfo_fake" `shouldReturn` Just Map.empty

  it "says nothing about a package the plan does not hold" $
    withFakeProject [("src/M.hs", "module M where\n")] $
      \rs -> askFixities rs "Paths_not_a_package" `shouldReturn` Nothing

  it "lets a module that imports one be read"
    $ withFakeProject
      [ ( "src/Facade.hs",
          "module Facade ((<+>)) where\nimport Paths_fake\nimport Inner\n"
        ),
        ("src/Inner.hs", "module Inner ((<+>)) where\ninfixr 5 <+>\na <+> b = a\n")
      ]
    $ \rs ->
      askFixities rs "Facade"
        >>= (`shouldBe` Just (Map.singleton (InTerms, OpName "<+>") (Fixity RightAssoc 5)))

  it "reads one somebody wrote by hand rather than assuming"
    $ withFakeProject
      [ ( "src/Paths_fake.hs",
          "module Paths_fake ((<+>)) where\ninfixr 5 <+>\na <+> b = a\n"
        )
      ]
    $ \rs ->
      askFixities rs "Paths_fake"
        >>= (`shouldBe` Just (Map.singleton (InTerms, OpName "<+>") (Fixity RightAssoc 5)))

-- | A module of the kind @hsc2hs@ takes, declaring an operator nothing can
-- get at.
cursed :: Text
cursed =
  T.unlines
    [ "#include <signal.h>",
      "module Cursed (interrupt, (<+>)) where",
      "infixr 5 <+>",
      "(<+>) :: Int -> Int -> Int",
      "a <+> b = a + b",
      "interrupt :: Int",
      "interrupt = #const SIGINT"
    ]

-- | What @unix@ writes, in miniature: a fixity declaration for a name used
-- in backticks, in a file no reading of ours reaches.
signals :: Text
signals =
  T.unlines
    [ "#include <signal.h>",
      "module System.Posix.Signals (addSignal, deleteSignal) where",
      "infixr `addSignal`, `deleteSignal`",
      "addSignal :: Int -> Int -> Int",
      "addSignal s m = m + #const SIGINT"
    ]

-- | What the chase makes of one module's @<+>@, against a world of modules
-- that disagree about it.
chased :: Text -> IO (Maybe Fixity)
chased source = do
  answer <-
    withReexports (Is #implicitPrelude) reach carries Set.empty "M" (pmModule parsed)
  pure $ case answer of
    Declares fixities -> Map.lookup (InTerms, OpName "<+>") fixities
    Unreadable _ -> Nothing
  where
    carries m =
      pure $ case m of
        "Text.PrettyPrint" ->
          Map.fromList [(OpName "Doc", Set.fromList [OpName "<+>"])]
        _ -> Map.empty
    parsed = case parseModule defaultParserConfig "M.hs" source of
      Left _ -> error "the test input did not parse"
      Right m -> m
    reach m =
      pure $ case m of
        "Control.Arrow" -> Just (Map.fromList [((InTerms, OpName "<+>"), Fixity RightAssoc 5)])
        "Text.PrettyPrint" -> Just (Map.fromList [((InTerms, OpName "<+>"), Fixity LeftAssoc 6)])
        "Prelude" -> Just Map.empty
        _ -> Nothing

withPlan :: BuildPlan -> Spec
withPlan plan = do
  resolver <- runIO (newResolver plan)
  let resolve = askFixities resolver
      exported = askExportNames resolver

  describe "the plan itself" $ do
    it "names the compiler" $
      T.unpack (bpCompiler plan) `shouldSatisfy` isInfixOf "ghc-"

    it "has the dependencies a real project has" $
      length (bpPackages plan) `shouldSatisfy` (> 20)

    it "gives every fetchable package a source hash to check against" $ do
      let fetchable = filter isFetchable (bpPackages plan)
      filter (null . sourceHashOf) fetchable `shouldBe` []

    it "does not mark the project itself as fetchable" $ do
      let locals = filter (\p -> ppName p == "tilia") (bpPackages plan)
      filter isFetchable locals `shouldBe` []

    it "records the project as a local directory, with its path" $ do
      let locals = [s' | p <- bpPackages plan, ppName p == "tilia", let s' = ppSource p]
      locals `shouldSatisfy` all (\s' -> case s' of LocalPackage path -> not (null path); _ -> False)

    it "puts every package in exactly one of the three kinds" $ do
      let kinds p = length (filter id [isPreExisting p, isFetchable p, isLocal p])
          isPreExisting p = ppSource p == PreExisting
          isLocal p = case ppSource p of LocalPackage _ -> True; _ -> False
      filter ((/= 1) . kinds) (bpPackages plan) `shouldBe` []

  describe "resolving a module that declares its own operators" $ do
    it "finds <+> in prettyprinter, with the right fixity" $
      needs resolve "Prettyprinter.Internal" $ \fixities ->
        Map.lookup (InTerms, OpName "<+>") fixities `shouldBe` Just (Fixity RightAssoc 6)

    it "resolves the same module twice to the same answer" $
      needs resolve "Prettyprinter.Internal" $ \first' -> do
        again <- resolve "Prettyprinter.Internal"
        again `shouldBe` Just first'

  describe "re-exports" $
    it "finds an operator a module exports but does not declare" $
      needs resolve "Prettyprinter" $ \fixities ->
        Map.lookup (InTerms, OpName "<+>") fixities `shouldBe` Just (Fixity RightAssoc 6)

  describe "boot packages" $ do
    it "answers for Prelude from the built-in table" $
      needs resolve "Prelude" $ \fixities -> do
        Map.lookup (InTerms, OpName "$") fixities `shouldBe` Just (Fixity RightAssoc 0)
        Map.lookup (InTerms, OpName ">>=") fixities `shouldBe` Just (Fixity LeftAssoc 1)
        Map.lookup (InTerms, OpName ".") fixities `shouldBe` Just (Fixity RightAssoc 9)
        Map.lookup (InTerms, OpName ":") fixities `shouldBe` Just (Fixity RightAssoc 5)

    it "answers for Control.Applicative" $
      needs resolve "Control.Applicative" $ \fixities ->
        Map.lookup (InTerms, OpName "<|>") fixities `shouldBe` Just (Fixity LeftAssoc 3)

    it "covers the containers and text modules a project actually imports" $ do
      let expected =
            [ ("Data.Map", "!", Fixity LeftAssoc 9),
              ("Data.Map", "\\\\", Fixity LeftAssoc 9),
              ("Data.Set", "\\\\", Fixity LeftAssoc 9),
              ("Data.Sequence", "|>", Fixity LeftAssoc 5),
              ("Data.Sequence", "<|", Fixity RightAssoc 5),
              ("Data.Bits", ".&.", Fixity LeftAssoc 7),
              ("Data.Ratio", "%", Fixity LeftAssoc 7),
              ("Data.Functor", "<&>", Fixity LeftAssoc 1),
              ("Control.Monad", ">=>", Fixity RightAssoc 1),
              ("Data.Semigroup", "<>", Fixity RightAssoc 6)
            ]
      wrong <- traverse (check resolve) expected
      concat wrong `shouldBe` []

    it "carries re-exports already resolved" $ do
      p <- resolve "Prelude"
      m <- resolve "Data.Map"
      ( Map.lookup (InTerms, OpName "$") =<< p,
        Map.lookup (InTerms, OpName "!") =<< m
        )
        `shouldBe` (Just (Fixity RightAssoc 0), Just (Fixity LeftAssoc 9))

    it "gives the same operator different fixities in different modules" $ do
      inList <- resolve "Data.List"
      inMap <- resolve "Data.Map"
      ( Map.lookup (InTerms, OpName "\\\\") =<< inList,
        Map.lookup (InTerms, OpName "\\\\") =<< inMap
        )
        `shouldBe` (Just (Fixity NoAssoc 5), Just (Fixity LeftAssoc 9))

  describe "modules it cannot answer for" $ do
    it "says so rather than claiming no operators" $
      resolve "Not.A.Real.Module.At.All" `shouldReturn` Nothing

    it "says so for a module no package exposes" $
      resolve "Some.Package.That.Does.Not.Exist" `shouldReturn` Nothing

    it "distinguishes a boot module with no operators from an unknown one" $ do
      quiet <- resolve "Data.Char"
      quiet `shouldBe` Just Map.empty

  describe "a module that leans on its package's extensions" $ do
    it "reads it, given what the .cabal puts in force" $
      withFakeProject [("fake.cabal", package ["LambdaCase"]), ("src/Fancy.hs", fancy)] $
        \rs ->
          askFixities rs "Fancy"
            >>= (`shouldBe` Just (Map.singleton (InTerms, OpName "<+>") (Fixity RightAssoc 5)))

    it "cannot read it when the .cabal puts nothing in force" $
      withFakeProject [("fake.cabal", package []), ("src/Fancy.hs", fancy)] $
        \rs -> askFixities rs "Fancy" `shouldReturn` Nothing

    it "takes an extension the .cabal turns off into account"
      $ withFakeProject
        [ ("fake.cabal", package ["LambdaCase", "NoLambdaCase"]),
          ("src/Fancy.hs", fancy)
        ]
      $ \rs -> askFixities rs "Fancy" `shouldReturn` Nothing

  describe "modules whose source defeats us" $ do
    it "answers for Test.QuickCheck.Property, which cannot be parsed" $
      needs resolve "Test.QuickCheck.Property" $ \fixities -> do
        Map.lookup (InTerms, OpName "===") fixities `shouldBe` Just (Fixity NoAssoc 4)
        Map.lookup (InTerms, OpName ".&&.") fixities `shouldBe` Just (Fixity RightAssoc 1)
        Map.lookup (InTerms, OpName "==>") fixities `shouldBe` Just (Fixity RightAssoc 0)

    it "carries that through the re-export chain to Test.QuickCheck" $
      needs resolve "Test.QuickCheck" $ \fixities ->
        Map.lookup (InTerms, OpName "===") fixities `shouldBe` Just (Fixity NoAssoc 4)

  describe "a module with more than one configuration" $ do
    -- Built in a temporary directory with a build plan written by hand, so
    -- that the shapes below can be exactly the shapes worth testing. These
    -- are the ones criterion's dependencies turned out to be written in.
    it "answers from the configurations it can read"
      $ withFakeProject
        [ ( "src/Platform.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "module Platform (sort, (<+>)) where",
                "#ifdef WINDOWS",
                "import No.Such.Module.At.All",
                "#endif",
                "import Data.List (sort)",
                "infixl 6 <+>",
                "(<+>) :: Int -> Int -> Int",
                "a <+> b = a + b"
              ]
          )
        ]
      $ \rs ->
        -- The WINDOWS branch imports a module nothing has, which is what
        -- System.IO.CodePage does with System.Win32.CodePage. That branch
        -- is passed over rather than taken as a reason to say nothing.
        askFixities rs "Platform"
          >>= (`shouldBe` Just (Map.singleton (InTerms, OpName "<+>") (Fixity LeftAssoc 6)))

    it "answers from the configurations that are Haskell at all"
      $ withFakeProject
        [ ( "src/Guarded.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "module Guarded ((<+>)) where",
                "infixl 6 <+>",
                "(<+>) :: Int -> Int -> Int",
                "a <+> b = a + b",
                "#ifdef ANCIENT",
                "f x = case",
                "#endif"
              ]
          )
        ]
      $ \rs ->
        askFixities rs "Guarded"
          >>= (`shouldBe` Just (Map.singleton (InTerms, OpName "<+>") (Fixity LeftAssoc 6)))

    it "still refuses when the configurations it can read disagree"
      $ withFakeProject
        [ ( "src/Disagree.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "module Disagree (sort, (<+>)) where",
                "import Data.List (sort)",
                "#ifdef FAST",
                "infixl 6 <+>",
                "#else",
                "infixr 7 <+>",
                "#endif",
                "(<+>) :: Int -> Int -> Int",
                "a <+> b = a + b"
              ]
          )
        ]
      $ \rs -> askFixities rs "Disagree" `shouldReturn` Nothing

    it "says nothing when it can read no configuration at all"
      $ withFakeProject
        [ ( "src/Bothbad.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "module Bothbad (sort) where",
                "#ifdef WINDOWS",
                "import No.Such.One",
                "#else",
                "import No.Such.Two",
                "#endif",
                "import Data.List (sort)"
              ]
          )
        ]
      $ \rs ->
        -- Not @Just mempty@: that would be claiming the module declares
        -- nothing, which is a guess rather than the silence it deserves.
        askFixities rs "Bothbad" `shouldReturn` Nothing

  describe "modules that re-export one another" $
    it "answers for one whose re-exports are mutually entangled" $ do
      answer <- resolve "GHC.Hs"
      answer `shouldSatisfy` (/= Nothing)

  describe "what a package module says it exports" $
    it "names them, read out of the package's own tarball" $
      exported "Prettyprinter" >>= \case
        Nothing -> pendingWith "could not read prettyprinter's source"
        Just names -> names `shouldSatisfy` Set.member (OpName "<+>")

  describe "what a module keeps under each of its names" $ do
    it "reads them out of a package's interface" $ do
      kept <- askChildren resolver "Data.List.NonEmpty"
      Map.lookup (OpName "NonEmpty") kept
        `shouldSatisfy` maybe False (Set.member (OpName ":|"))

    it "has nothing to say about a module it cannot find" $
      askChildren resolver "No.Such.Module" `shouldReturn` Map.empty

    it "reads them out of a local module's source"
      $ withFakeProject
        [("src/Carrier.hs", "module Carrier (T (..)) where\ndata T = A | Int :| Int\n")]
      $ \rs -> do
        kept <- askChildren rs "Carrier"
        Map.lookup (OpName "T") kept
          `shouldBe` Just (Set.fromList [OpName "A", OpName ":|"])

    it "follows a type to the module that declares it"
      $ withFakeProject
        [ ("src/Facade.hs", "module Facade (T (..)) where\nimport Inner\n"),
          ("src/Inner.hs", "module Inner (T (..)) where\ninfixr 5 :|\ndata T = A | Int :| Int\n")
        ]
      $ \rs -> do
        kept <- askChildren rs "Facade"
        Map.lookup (OpName "T") kept
          `shouldBe` Just (Set.fromList [OpName "A", OpName ":|"])

    it "carries the fixity along with it, so the name can be looked up"
      $ withFakeProject
        [ ("src/Facade.hs", "module Facade (T (..)) where\nimport Inner\n"),
          ("src/Inner.hs", "module Inner (T (..)) where\ninfixr 5 :|\ndata T = A | Int :| Int\n")
        ]
      $ \rs -> do
        fixities <- askFixities rs "Facade"
        (Map.lookup (InTerms, OpName ":|") =<< fixities)
          `shouldBe` Just (Fixity RightAssoc 5)

    it "settles an operator that arrives through a façade"
      $ withFakeProject
        [ ("src/Facade.hs", "module Facade (T (..)) where\nimport Inner\n"),
          ("src/Inner.hs", "module Inner (T (..)) where\ninfixr 5 :|\ndata T = A | Int :| Int\n")
        ]
      $ \rs -> do
        let m = parse "module M where\nimport Facade (T (..))\n"
        scope <- scopeFor rs (Is #implicitPrelude) (pmModule m)
        lookupFixity scope InTerms Nothing (OpName ":|")
          `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Facade")

    it "follows a whole module handed on"
      $ withFakeProject
        [ ("src/Facade.hs", "module Facade (module Inner) where\nimport Inner\n"),
          ("src/Inner.hs", "module Inner (T (..)) where\ninfixr 5 :|\ndata T = A | Int :| Int\n")
        ]
      $ \rs -> do
        kept <- askChildren rs "Facade"
        Map.lookup (OpName "T") kept
          `shouldBe` Just (Set.fromList [OpName "A", OpName ":|"])

    it "comes back from two modules that hand each other on"
      $ withFakeProject
        [ ("src/Ping.hs", "module Ping (T (..)) where\nimport Pong\n"),
          ("src/Pong.hs", "module Pong (T (..)) where\nimport Ping\n")
        ]
      $ \rs -> askChildren rs "Ping" `shouldReturn` Map.singleton (OpName "T") Set.empty

    it "keeps to what a local module's export list hands on"
      $ withFakeProject
        [("src/Carrier.hs", "module Carrier (T (A)) where\ndata T = A | Int :| Int\n")]
      $ \rs -> do
        kept <- askChildren rs "Carrier"
        Map.lookup (OpName "T") kept `shouldBe` Just (Set.singleton (OpName "A"))

  describe "what a module says it exports, where its fixities are beyond us" $ do
    it "names them though the module itself went unresolved" $
      withFakeProject [("src/Opaque.hs", opaqueSource)] $
        \rs -> do
          askFixities rs "Opaque" `shouldReturn` Nothing
          askExportNames rs "Opaque"
            `shouldReturn` Just (Set.fromList [OpName "<+>", OpName "f"])

    it "says nothing for a module that hands a whole module on"
      $ withFakeProject
        [ ( "src/Wide.hs",
            T.unlines
              [ "module Wide (module Data.List) where",
                "import Data.List",
                "import No.Such.Module"
              ]
          )
        ]
      $ \rs -> askExportNames rs "Wide" `shouldReturn` Nothing

    it "says nothing for a module it cannot find at all" $
      withFakeProject [("src/Opaque.hs", opaqueSource)] $
        \rs -> askExportNames rs "No.Such.Module" `shouldReturn` Nothing

    it "follows a type it hands on to the module that declares it"
      $ withFakeProject
        [ ("src/Facade.hs", "module Facade (T (..), (<+>)) where\nimport Inner\n"),
          ("src/Inner.hs", "module Inner (T (..)) where\ndata T = A | Int :| Int\n")
        ]
      $ \rs ->
        askExportNames rs "Facade"
          `shouldReturn` Just (Set.fromList [OpName "T", OpName "A", OpName ":|", OpName "<+>"])

    it "follows a whole module it hands on"
      $ withFakeProject
        [ ("src/Facade.hs", "module Facade (module Inner) where\nimport Inner\n"),
          ("src/Inner.hs", "module Inner ((<+>)) where\ninfixl 6 <+>\n(<+>) :: Int -> Int -> Int\na <+> b = a + b\n")
        ]
      $ \rs ->
        askExportNames rs "Facade" `shouldReturn` Just (Set.singleton (OpName "<+>"))

    it "says nothing when what it hands on cannot be read"
      $ withFakeProject
        [("src/Facade.hs", "module Facade (module No.Such.Module) where\nimport No.Such.Module\n")]
      $ \rs -> askExportNames rs "Facade" `shouldReturn` Nothing

    it "says nothing when a type it hands on is beyond us"
      $ withFakeProject
        [("src/Facade.hs", "module Facade (T (..)) where\nimport No.Such.Module\n")]
      $ \rs -> askExportNames rs "Facade" `shouldReturn` Nothing

    it "comes back from two modules that hand each other on"
      $ withFakeProject
        [ ("src/Ping.hs", "module Ping (module Pong) where\nimport Pong\n"),
          ("src/Pong.hs", "module Pong (module Ping) where\nimport Ping\n")
        ]
      $ \rs -> askExportNames rs "Ping" `shouldReturn` Nothing

    it "says nothing for a module whose source will not parse" $
      withFakeProject [("src/Bad.hs", "module Bad ((<+>)) where\nf = (((\n")] $
        \rs -> askExportNames rs "Bad" `shouldReturn` Nothing

    it "takes them from every configuration the preprocessor allows"
      $ withFakeProject
        [ ( "src/Both.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "#ifdef WINDOWS",
                "module Both ((<+>)) where",
                "#else",
                "module Both ((<?>)) where",
                "#endif",
                "import No.Such.Module"
              ]
          )
        ]
      $ \rs ->
        askExportNames rs "Both" `shouldReturn` Just (Set.fromList [OpName "<+>", OpName "<?>"])

    it "says nothing when one configuration hands a whole module on"
      $ withFakeProject
        [ ( "src/Half.hs",
            T.unlines
              [ "{-# LANGUAGE CPP #-}",
                "#ifdef WINDOWS",
                "module Half ((<+>)) where",
                "#else",
                "module Half (module Data.List) where",
                "#endif",
                "import Data.List",
                "import No.Such.Module"
              ]
          )
        ]
      $ \rs -> askExportNames rs "Half" `shouldReturn` Nothing

    it "settles an operator no unread module in scope could have declared" $
      withFakeProject [("src/Opaque.hs", opaqueSource)] $
        \rs -> do
          let m = parse "module M where\nimport Opaque\n"
          scope <- scopeFor rs (Is #implicitPrelude) (pmModule m)
          lookupFixity scope InTerms Nothing (OpName "<??>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "leaves one alone that the unread module's list does name" $
      withFakeProject [("src/Opaque.hs", opaqueSource)] $
        \rs -> do
          let m = parse "module M where\nimport Opaque\n"
          scope <- scopeFor rs (Is #implicitPrelude) (pmModule m)
          lookupFixity scope InTerms Nothing (OpName "<+>")
            `shouldBe` Unresolved (ModuleChain ("Opaque" :| ["No.Such.Module"]) :| [])

    it "names the module that stopped it rather than the import above it" $
      withFakeProject [("src/Opaque.hs", opaqueSource)] $
        \rs -> askChain rs "Opaque" `shouldReturn` ["No.Such.Module"]

    it "follows the reasons down more than one module"
      $ withFakeProject
        [ ("src/Near.hs", "module Near ((<+>)) where\nimport Middle\n"),
          ("src/Middle.hs", "module Middle ((<+>)) where\nimport No.Such.Module\n")
        ]
      $ \rs -> askChain rs "Near" `shouldReturn` ["Middle", "No.Such.Module"]

    it "has nothing to say about a module that could be read" $
      withFakeProject [("src/Opaque.hs", opaqueSource)] $
        \rs -> askChain rs "Prelude" `shouldReturn` []

  describe "the whole pipeline, from source text to a fixity" $ do
    it "resolves an operator through a real import" $
      endToEnd resolver "module M where\nimport Prettyprinter\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "prefers the module's own declaration to an imported one" $
      endToEnd resolver "module M where\nimport Prettyprinter\ninfixl 2 <+>\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity LeftAssoc 2) DeclaredHere

    it "honours a qualified import" $
      endToEnd resolver "module M where\nimport qualified Prettyprinter as P\n" $ \scope -> do
        lookupFixity scope InTerms (Just "P") (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")
        -- Qualified-only, so nothing arrives unqualified.
        lookupFixity scope InTerms Nothing (OpName "<+>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "honours an explicit import list" $
      endToEnd resolver "module M where\nimport Prettyprinter ((<+>))\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "honours a hiding list" $
      endToEnd resolver "module M where\nimport Prettyprinter hiding ((<+>))\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<+>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "concludes the Report default when everything in scope was read" $
      endToEnd resolver "module M where\nimport Prettyprinter\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<!@#>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude anything when an import could not be read" $
      endToEnd resolver "module M where\nimport No.Such.Module\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<!@#>")
          `shouldBe` Unresolved (unreadOnly "No.Such.Module")

    it "still answers for what it did find, despite an unreadable import" $
      endToEnd resolver "module M where\nimport Prettyprinter\nimport No.Such.Module\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "concludes the default through a boot import that exports no operators" $
      endToEnd resolver "module M where\nimport Data.Char\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "<!@#>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "resolves an operator imported from a boot package" $
      endToEnd resolver "module M where\nimport Data.Map\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName "!")
          `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "resolves an operator that arrives under a type's own name" $
      endToEnd resolver "module M where\nimport Data.List.NonEmpty (NonEmpty (..))\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName ":|")
          `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Data.List.NonEmpty")

    it "resolves one written out beside its type" $
      endToEnd resolver "module M where\nimport Data.List.NonEmpty (NonEmpty ((:|)))\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName ":|")
          `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Data.List.NonEmpty")

    it "leaves out an operator no item of the list brings in" $
      endToEnd resolver "module M where\nimport Data.List.NonEmpty (toList)\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName ":|")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "hides one hidden along with its type" $
      endToEnd resolver "module M where\nimport Data.List.NonEmpty hiding (NonEmpty (..))\n" $ \scope ->
        lookupFixity scope InTerms Nothing (OpName ":|")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "brings one in under the qualifier it was imported with" $
      endToEnd resolver "module M where\nimport qualified Data.List.NonEmpty as NE (NonEmpty (..))\n" $ \scope ->
        lookupFixity scope InTerms (Just "NE") (OpName ":|")
          `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Data.List.NonEmpty")

    it "reports no ambiguity for a module that compiles" $
      endToEnd resolver "module M where\nimport Prettyprinter\n" $ \scope ->
        reachAmbiguous (scopeInTerms scope) `shouldBe` []

  describe "readiness" $ do
    it "reports something other than a missing plan for this project" $ do
      readiness <- checkReadiness [] "."
      readiness `shouldNotBe` PlanMissing

    it "reports a missing plan for a directory that has none" $
      checkReadiness [] "/" `shouldReturn` PlanMissing

----------------------------------------------------------------------------
-- Helpers

-- | An empty project directory, holding a plan where @cabal@ writes one if
-- it is to hold a plan at all.
withTempProject :: Maybe Text -> (FilePath -> IO a) -> IO a
withTempProject plan act =
  withSystemTempDirectory "tilia-prepare" $ \dir -> do
    createDirectoryIfMissing True (takeDirectory (planPathFor dir))
    traverse_ (writePlan dir) plan
    act dir

writePlan :: FilePath -> Text -> IO ()
writePlan dir = T.writeFile (planPathFor dir)

-- | A plan naming one package that no package cache can have a tarball
-- for, so that reading it leaves something to fetch.
wantingATarball :: Text
wantingATarball =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"tilia-phantom\",\"pkg-version\":\"9.9.9\",\
  \\"pkg-src\":{\"type\":\"repo-tar\"}}]}"

-- | A plan holding a local package with a library and an executable, and
-- no test suite.
twoComponents :: Text
twoComponents =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\"component-name\":\"lib\",\
  \\"pkg-src\":{\"type\":\"local\",\"path\":\"/nowhere\"}},\
  \{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\"component-name\":\"exe:thing\",\
  \\"pkg-src\":{\"type\":\"local\",\"path\":\"/nowhere\"}}]}"

-- | The same, with a component a solve could go on to add.
threeComponents :: Text
threeComponents =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\"component-name\":\"lib\",\
  \\"pkg-src\":{\"type\":\"local\",\"path\":\"/nowhere\"}},\
  \{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\"component-name\":\"exe:thing\",\
  \\"pkg-src\":{\"type\":\"local\",\"path\":\"/nowhere\"}},\
  \{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\"component-name\":\"test:tests\",\
  \\"pkg-src\":{\"type\":\"local\",\"path\":\"/nowhere\"}}]}"

-- | Narrow and short at once: two components, neither of them the one a
-- run wants, and a dependency whose tarball is nowhere.
--
-- The shape @servant@ has, where two cookbook executables are named by the
-- project and left out of every plan @cabal@ writes.
narrowAndWanting :: Text
narrowAndWanting =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\"component-name\":\"lib\",\
  \\"pkg-src\":{\"type\":\"local\",\"path\":\"/nowhere\"}},\
  \{\"pkg-name\":\"tilia-phantom\",\"pkg-version\":\"9.9.9\",\
  \\"pkg-src\":{\"type\":\"repo-tar\"}}]}"

-- | A package planned whole, as @cabal@ plans one with a @Custom@ build
-- type: no @component-name@, and a @components@ object instead.
plannedWhole :: Text
plannedWhole =
  "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
  \[{\"pkg-name\":\"thing\",\"pkg-version\":\"1.0\",\"type\":\"configured\",\
  \\"pkg-src\":{\"type\":\"local\",\"path\":\"/nowhere\"},\
  \\"components\":{\"lib\":{},\"test:spec\":{},\"setup\":{}}}]}"

-- | A component of that package, by the name a plan gives it.
component :: Text -> PlanComponent
component = PlanComponent "thing"

-- | Note that @cabal@ was asked for something.
-- | What a solve and a fetch are asked for.
--
-- Both name the test suites and the benchmarks, because both are things a
-- run formats and so things the plan has to reach.
solving, fetching :: [String]
solving = narrowSolve <> wholeProject
fetching = narrowFetch <> wholeProject

-- | The same two, asked only about what @cabal@ builds by default. What is
-- fallen back on where the whole project will not solve.
narrowSolve, narrowFetch :: [String]
narrowSolve = ["build", "all", "--dry-run"]
narrowFetch = ["build", "all", "--only-download"]

wholeProject :: [String]
wholeProject = ["--enable-tests", "--enable-benchmarks"]

record :: IORef [[String]] -> [String] -> IO ()
record steps args = modifyIORef' steps (<> [args])

-- | A @cabal@ that does nothing and says it went well.
obliging :: IORef [[String]] -> [String] -> IO (Either Text ())
obliging steps args = record steps args >> pure (Right ())

-- | Run an assertion on a module's fixities, or mark the test pending if
-- the module could not be resolved at all.
--
-- Pending rather than failing, because an unpopulated package cache is an
-- environment problem and not a defect in the code under test.
needs ::
  (Text -> IO (Maybe (Fixities))) ->
  Text ->
  (Fixities -> Expectation) ->
  Expectation
needs resolve modName assertion =
  resolve modName >>= \case
    Nothing -> pendingWith ("could not resolve " <> T.unpack modName)
    Just fixities -> assertion fixities

-- | Parse a module, resolve its imports for real, and hand over the scope.
endToEnd ::
  Resolver ->
  Text ->
  (Scope -> Expectation) ->
  Expectation
endToEnd resolver source assertion =
  case parseModule defaultParserConfig "test.hs" source of
    Left _ -> expectationFailure "the test input did not parse"
    Right pm -> do
      scope <- scopeFor resolver (Is #implicitPrelude) (pmModule pm)
      assertion scope

-- | Check one expected fixity, returning a description of any mismatch.
check ::
  (Text -> IO (Maybe (Fixities))) ->
  (Text, Text, Fixity) ->
  IO [String]
check resolve (modName, op, expected) = do
  got <- resolve modName
  let actual = Map.lookup (InTerms, OpName op) =<< got
  pure
    [ T.unpack modName
        <> "."
        <> T.unpack op
        <> ": expected "
        <> show expected
        <> " but got "
        <> show actual
    | actual /= Just expected
    ]

-- | A module that needs @LambdaCase@ to parse, and declares a fixity worth
-- finding once it does.
fancy :: Text
fancy =
  T.unlines
    [ "module Fancy where",
      "infixr 5 <+>",
      "(<+>) :: Int -> Int -> Int",
      "a <+> b = a + b",
      "describe :: Int -> Int",
      "describe = \\case",
      "  0 -> 1",
      "  _ -> 2"
    ]

-- | A @.cabal@ for the fake project, putting the named extensions in force.
package :: [Text] -> Text
package extensions =
  T.unlines $
    [ "cabal-version: 2.4",
      "name: fake",
      "version: 0.1.0.0",
      "library",
      "  exposed-modules: Fancy",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010"
    ]
      <> ["  default-extensions: " <> T.intercalate ", " extensions | not (null extensions)]

-- | A module that is perfectly readable and still cannot be resolved: the
-- operator it exports comes from somewhere nothing can be read from.
opaqueSource :: Text
opaqueSource =
  T.unlines
    [ "module Opaque ((<+>), f) where",
      "import No.Such.Module",
      "f :: Int",
      "f = 1"
    ]

parse :: Text -> ParsedModule
parse source = case parseModule defaultParserConfig "M.hs" source of
  Left _ -> error "the test input did not parse"
  Right pm -> pm

-- | A project of made-up modules, with a build plan written by hand.
--
-- The plan names one local package and nothing else, which is enough for a
-- resolver: local modules are read straight off disk, and everything they
-- import here is either a boot module or does not exist.
withFakeProject :: [(FilePath, Text)] -> (Resolver -> IO a) -> IO a
withFakeProject sources act =
  withFakePlan sources (\plan -> newResolver plan >>= act)

withFakePlan :: [(FilePath, Text)] -> (BuildPlan -> IO a) -> IO a
withFakePlan sources act =
  withSystemTempDirectory "tilia-plan" $ \dir -> do
    createDirectoryIfMissing True (dir </> "src")
    if any ((".cabal" `Data.List.isSuffixOf`) . fst) sources
      then pure ()
      else
        T.writeFile (dir </> "fake.cabal") $
          T.unlines
            [ "cabal-version: 2.4",
              "name: fake",
              "version: 0.1.0.0",
              "library",
              "  exposed-modules: " <> T.intercalate ", " (fmap named (haskellIn sources)),
              "  hs-source-dirs: src",
              "  default-language: Haskell2010"
            ]
    traverse_ (\(path, text) -> T.writeFile (dir </> path) text) sources
    T.writeFile (dir </> "plan.json") $
      "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
      \[{\"pkg-name\":\"fake\",\"pkg-version\":\"0.1.0.0\",\
      \\"pkg-src\":{\"type\":\"local\",\"path\":\""
        <> T.pack dir
        <> "\"}}]}"
    readBuildPlan (dir </> "plan.json") >>= \case
      Left why -> error (T.unpack why)
      Right plan -> act plan
  where
    haskellIn = filter (isModule . fst)
    isModule path = any (`Data.List.isSuffixOf` path) [".hs", ".hsc"]
    named (path, _) = T.pack (takeBaseName path)

-- | A project whose one dependency is a package off Hackage, with the
-- tarball @cabal@ would have fetched written where it would have put it.
--
-- Nothing here is local: this is the other route to a module, the one that
-- opens an archive. @CABAL_DIR@ says where the package cache is and
-- @XDG_CACHE_HOME@ where what is read gets remembered, so the run reaches
-- the tarball below and no further, and leaves nothing behind.
withFakeArchive :: [(FilePath, Text)] -> (Resolver -> IO a) -> IO a
withFakeArchive sources act =
  withSystemTempDirectory "tilia-archive" $ \dir -> do
    let held = T.unpack (name <> "-" <> version)
        tarball =
          dir
            </> "packages"
            </> "hackage.haskell.org"
            </> T.unpack name
            </> T.unpack version
            </> held
              <> ".tar.gz"
    createDirectoryIfMissing True (takeDirectory tarball)
    writeTarball tarball $
      (held </> T.unpack name <> ".cabal", cabal)
        : [(held </> path, text) | (path, text) <- sources]
    T.writeFile (dir </> "plan.json") plan
    withEnvironment [("CABAL_DIR", dir), ("XDG_CACHE_HOME", dir </> "cache")] $
      readBuildPlan (dir </> "plan.json") >>= \case
        Left why -> error (T.unpack why)
        Right p -> newResolver p >>= act
  where
    name = "tilia-hsc-fixture"
    version = "1.0"
    cabal =
      T.unlines
        [ "cabal-version: 2.4",
          "name: " <> name,
          "version: " <> version,
          "library",
          "  exposed-modules: " <> T.intercalate ", " (fmap moduleIn sources),
          "  hs-source-dirs: .",
          "  default-language: Haskell2010"
        ]
    moduleIn (path, _) = T.replace "/" "." (T.pack (dropExtension path))
    plan =
      "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":\
      \[{\"pkg-name\":\""
        <> name
        <> "\",\"pkg-version\":\""
        <> version
        <> "\",\"pkg-src\":{\"type\":\"repo-tar\"}}]}"

-- | Write the named files as a gzipped tarball.
writeTarball :: FilePath -> [(FilePath, Text)] -> IO ()
writeTarball path entries =
  BL.writeFile path . GZip.compress . Tar.write =<< traverse entry entries
  where
    entry (inside, text) = case Tar.toTarPath False inside of
      Left why -> error why
      Right tarPath -> pure (Tar.fileEntry tarPath (BL.fromStrict (T.encodeUtf8 text)))

-- | Run something with the given variables set, and the environment as it
-- was afterwards however it turns out.
withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment vars act = bracket set restore (const act)
  where
    set = traverse remember vars
    remember (key, value) = do
      was <- lookupEnv key
      setEnv key value
      pure (key, was)
    restore = traverse_ (\(key, was) -> maybe (unsetEnv key) (setEnv key) was)

-- | The one import blamed for an operator, unread on its own account and so
-- with nothing below it.
unreadOnly :: Text -> NonEmpty ModuleChain
unreadOnly m = ModuleChain (m :| []) :| []
