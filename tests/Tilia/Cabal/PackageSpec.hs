{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Information obtained from @.cabal@ files.
module Tilia.Cabal.PackageSpec (spec) where

import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.LanguageExtensions.Type (Extension (..))
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Cabal.Package (PackageProblem (..), newPackageReader)
import Tilia.Fixity.Plan (BuildPlan (..), plannedCompiler)

spec :: Spec
spec = do
  describe "the component a file belongs to" $ do
    it "is the one whose source directory holds it" $
      inPackage twoComponents ["src", "test"] $ \root -> do
        library <- asked (root </> "src" </> "M.hs")
        suite <- asked (root </> "test" </> "S.hs")
        (has ImportQualifiedPost library, has ImportQualifiedPost suite)
          `shouldBe` (True, False)

    it "settles the extensions separately for each" $
      inPackage twoComponents ["src", "test"] $ \root -> do
        library <- asked (root </> "src" </> "M.hs")
        suite <- asked (root </> "test" </> "S.hs")
        (has OverloadedStrings library, has OverloadedStrings suite)
          `shouldBe` (False, True)

    it "is none of them when the file is outside every source directory" $
      inPackage twoComponents ["src", "test", "scratch"] $ \root ->
        (unclaimed <$> asked (root </> "scratch" </> "X.hs"))
          `shouldReturn` True

    it "is the package's own directory when it names no source directory" $
      inPackage besideTheCabalFile [] $ \root ->
        (has ImportQualifiedPost <$> asked (root </> "M.hs"))
          `shouldReturn` True

    it "is the nearer one when a wider component covers it as well" $
      inPackage overTheWholeTree ["tests"] $ \root ->
        (has ImportQualifiedPost <$> asked (root </> "tests" </> "S.hs"))
          `shouldReturn` True

    it "is still the wider one for a file only it covers" $
      inPackage overTheWholeTree ["tests"] $ \root ->
        (has ImportQualifiedPost <$> asked (root </> "M.hs"))
          `shouldReturn` False

    it "prefers the component that names a module in a shared source directory" $
      inPackage sharedTests ["tests"] $ \root -> do
        unit <- asked (root </> "tests" </> "Unit.hs")
        driver <- asked (root </> "tests" </> "Doctests.hs")
        (has BangPatterns unit, has BangPatterns driver) `shouldBe` (True, False)

    it "inherits settings in conditional source directories" $
      inPackage conditionalLibrary ["src", "new"] $ \root ->
        (has BangPatterns <$> asked (root </> "new" </> "M.hs"))
          `shouldReturn` True

  describe "the setup script" $ do
    it "takes the compiler's defaults, not those of a component around it" $
      inPackage aroundTheSetupScript [] $ \root -> do
        library <- asked (root </> "M.hs")
        setup <- asked (root </> "Setup.hs")
        (has OverloadedStrings library, has OverloadedStrings setup)
          `shouldBe` (True, False)
        (has ImportQualifiedPost library, has ImportQualifiedPost setup)
          `shouldBe` (False, True)

    it "is settled when no component covers it" $
      inPackage twoComponents ["src", "test"] $ \root ->
        (has ImportQualifiedPost <$> asked (root </> "Setup.hs"))
          `shouldReturn` True

  describe "the extensions a component puts in force" $ do
    it "are the language edition's" $
      inPackage twoComponents ["src"] $ \root -> do
        library <- asked (root </> "src" </> "M.hs")
        (length <$> library) `shouldSatisfy` either (const False) (> 40)

    it "are Haskell98's where it names no edition, as cabal builds it" $
      inPackage namesNoEdition ["src"] $ \root -> do
        library <- asked (root </> "src" </> "M.hs")
        (has NondecreasingIndentation library, has ImportQualifiedPost library)
          `shouldBe` (True, False)

    it "can be turned off again by default-extensions" $
      inPackage refusesAnEdition ["src"] $ \root ->
        (has ImportQualifiedPost <$> asked (root </> "src" </> "M.hs"))
          `shouldReturn` False

    it "take in the extensions field older packages use, as cabal does" $
      inPackage usesTheOldField ["src"] $ \root ->
        (has Cpp <$> asked (root </> "src" </> "M.hs"))
          `shouldReturn` True

    it "take in those of every branch the build takes" $
      inPackage conditionalExtensions ["src"] $ \root -> do
        library <- asked (root </> "src" </> "M.hs")
        (has StarIsType library, has MagicHash library)
          `shouldBe` (False, True)

    it "leave out those of a branch the build does not take" $
      inPackage conditionalExtensions ["src"] $ \root -> do
        library <- asked (root </> "src" </> "M.hs")
        (has BangPatterns library, has Strict library)
          `shouldBe` (False, False)

    it "take the compiler's version from the plan where it names one" $
      inPackage conditionalExtensions ["src"] $ \root -> do
        ask <- newPackageReader (plannedCompiler (BuildPlan "ghc-8.4.4" []))
        library <- ask (root </> "src" </> "M.hs")
        (has StarIsType library, has BangPatterns library)
          `shouldBe` (True, True)

    it "take in their own branch's too, for a module only it declares" $
      inPackage conditionalExtensions ["src", "old"] $ \root -> do
        old <- asked (root </> "old" </> "Old.hs")
        (has BangPatterns old, has StarIsType old) `shouldBe` (True, False)

  describe "a file nothing can be settled for" $ do
    it "says so when there is no package above it" $
      withSystemTempDirectory "tilia-nopackage" $ \root ->
        asked (root </> "M.hs")
          `shouldReturn` Left NoPackageFile

    it "says so, and how, when the package does not parse" $
      inPackage "library\n  hs-source-dirs\n" [] $ \root ->
        asked (root </> "M.hs") >>= \case
          Left (PackageMalformed file complaints) -> do
            file `shouldSatisfy` (("demo.cabal" `isSuffixOf`))
            complaints `shouldSatisfy` not . null
          other -> expectationFailure ("expected a parse failure, got " <> show other)

  describe "a reader kept between files" $ do
    it "answers as a fresh one would" $
      inPackage twoComponents ["src", "test"] $ \root -> do
        ask <- newPackageReader Nothing
        kept <- traverse ask (modules root)
        fresh <- traverse asked (modules root)
        kept `shouldBe` fresh

    it "reads a package once, not once per file" $
      inPackage twoComponents ["src"] $ \root -> do
        ask <- newPackageReader Nothing
        first <- ask (root </> "src" </> "A.hs")
        removeFile (root </> "demo.cabal")
        ask (root </> "src" </> "B.hs") `shouldReturn` first

    it "remembers every directory the walk went through" $
      inPackage twoComponents ["src" </> "deep"] $ \root -> do
        ask <- newPackageReader Nothing
        deep <- ask (root </> "src" </> "deep" </> "A.hs")
        removeFile (root </> "demo.cabal")
        ask (root </> "src" </> "B.hs") `shouldReturn` deep

    it "is what makes those pass, and not the file surviving" $
      inPackage twoComponents ["src"] $ \root -> do
        removeFile (root </> "demo.cabal")
        (unreadableOrMissing <$> asked (root </> "src" </> "A.hs"))
          `shouldReturn` True

----------------------------------------------------------------------------
-- The packages the tests are run against

-- | A library on @GHC2021@ and a test suite on @Haskell2010@, so that the
-- two disagree about everything worth asking.
twoComponents :: Text
twoComponents =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "",
      "library",
      "  exposed-modules: M",
      "  hs-source-dirs: src",
      "  default-language: GHC2021",
      "",
      "test-suite spec",
      "  type: exitcode-stdio-1.0",
      "  main-is: S.hs",
      "  hs-source-dirs: test",
      "  default-language: Haskell2010",
      "  default-extensions: OverloadedStrings"
    ]

-- | A library that names no @hs-source-dirs@, so its modules sit beside the
-- @.cabal@ file.
besideTheCabalFile :: Text
besideTheCabalFile =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "",
      "library",
      "  exposed-modules: M",
      "  default-language: GHC2021"
    ]

-- | A library beside the @.cabal@ file with settings a setup script there
-- does not share.
aroundTheSetupScript :: Text
aroundTheSetupScript =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "",
      "library",
      "  exposed-modules: M",
      "  default-language: Haskell2010",
      "  default-extensions: OverloadedStrings"
    ]

-- | A library that spreads over the whole tree, and a suite inside it.
--
-- The library names no @hs-source-dirs@ and so takes the package
-- directory, which holds the test suite's directory as well as its own
-- modules.
overTheWholeTree :: Text
overTheWholeTree =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "",
      "library",
      "  exposed-modules: M",
      "  default-language: Haskell2010",
      "",
      "test-suite spec",
      "  type: exitcode-stdio-1.0",
      "  main-is: S.hs",
      "  hs-source-dirs: tests",
      "  default-language: GHC2021"
    ]

-- | A library that names no language edition.
namesNoEdition :: Text
namesNoEdition =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "",
      "library",
      "  exposed-modules: M",
      "  hs-source-dirs: src"
    ]

-- | An edition, and then one of the things it brings taken back out.
refusesAnEdition :: Text
refusesAnEdition =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "",
      "library",
      "  exposed-modules: M",
      "  hs-source-dirs: src",
      "  default-language: GHC2021",
      "  default-extensions: NoImportQualifiedPost"
    ]

-- | An extension turned on in the field that came before
-- @default-extensions@.
usesTheOldField :: Text
usesTheOldField =
  T.unlines
    [ "cabal-version: >= 1.10",
      "name: demo",
      "version: 0",
      "build-type: Simple",
      "",
      "library",
      "  exposed-modules: M",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  extensions: CPP"
    ]

-- | Extensions behind conditions on the compiler, one met and one not, and
-- behind a flag on by default and one off.
conditionalExtensions :: Text
conditionalExtensions =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "",
      "flag fast",
      "  default: True",
      "",
      "flag strict",
      "  default: False",
      "",
      "library",
      "  exposed-modules: M",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if impl(ghc >= 8.6)",
      "    default-extensions: NoStarIsType",
      "  if impl(ghc < 8.6)",
      "    other-modules: Old",
      "    hs-source-dirs: old",
      "    default-extensions: BangPatterns",
      "  if flag(fast)",
      "    default-extensions: MagicHash",
      "  if flag(strict)",
      "    default-extensions: Strict"
    ]

----------------------------------------------------------------------------
-- Running one

sharedTests :: Text
sharedTests =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "test-suite doctests",
      "  type: exitcode-stdio-1.0",
      "  main-is: Doctests.hs",
      "  hs-source-dirs: tests",
      "  default-language: Haskell2010",
      "test-suite unittests",
      "  type: exitcode-stdio-1.0",
      "  main-is: Unittests.hs",
      "  other-modules: Unit",
      "  hs-source-dirs: tests",
      "  default-language: Haskell2010",
      "  default-extensions: BangPatterns"
    ]

conditionalLibrary :: Text
conditionalLibrary =
  T.unlines
    [ "cabal-version: 2.4",
      "name: demo",
      "version: 0",
      "library",
      "  exposed-modules: M",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  default-extensions: BangPatterns",
      "  if impl(ghc >= 9.10)",
      "    hs-source-dirs: new"
    ]

-- | Write a @.cabal@ file and the given directories, and hand back the root.
inPackage :: Text -> [FilePath] -> (FilePath -> IO a) -> IO a
inPackage contents dirs use =
  withSystemTempDirectory "tilia-package" $ \root -> do
    T.writeFile (root </> "demo.cabal") contents
    mapM_ (createDirectoryIfMissing True . (root </>)) (sort dirs)
    use root

-- | Ask about one file with a reader of its own, which is what a test that
-- is not about caching wants.
asked :: FilePath -> IO (Either PackageProblem [Extension])
asked path = do
  ask <- newPackageReader Nothing
  ask path

-- | One module in each of the two components.
modules :: FilePath -> [FilePath]
modules root = [root </> "src" </> "M.hs", root </> "test" </> "S.hs"]

unreadableOrMissing :: Either PackageProblem [Extension] -> Bool
unreadableOrMissing = either (const True) (const False)

has :: Extension -> Either PackageProblem [Extension] -> Bool
has e = either (const False) (e `elem`)

unclaimed :: Either PackageProblem [Extension] -> Bool
unclaimed = either isFileUnclaimed (const False)
  where
    isFileUnclaimed = \case
      FileUnclaimed _ -> True
      _ -> False
