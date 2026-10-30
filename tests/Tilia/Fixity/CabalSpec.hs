{-# LANGUAGE OverloadedStrings #-}

-- | Reading a package's description out of its @.cabal@ file and its
-- tarball.
module Tilia.Fixity.CabalSpec (spec) where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Distribution.PackageDescription (GenericPackageDescription)
import GHC.LanguageExtensions.Type (Extension (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Cabal.Package
  ( declaredModules,
    describePackageProblem,
    packageBranches,
    packageExtensions,
    packageSourceDirs,
    parsePackageDescription,
  )
import Tilia.Fixity.Archive (Archive (..), InArchive (..), moduleIn)
import Tilia.Fixity.Cabal
import Tilia.Pragma (effectiveExtensions)

spec :: Spec
spec = do
  describe "finding the cabal file in an archive" $ do
    it "spells an entry's path the way the archive holds it" $
      entryPosixPath (entryFor "hspec-2.11.17/hspec.cabal" "")
        `shouldBe` "hspec-2.11.17/hspec.cabal"

    it "is the same spelling on every machine" $
      entryPosixPath (entryFor "hspec-2.11.17/hspec.cabal" "")
        `shouldBe` Tar.fromTarPathToPosixPath
          (Tar.entryTarPath (entryFor "hspec-2.11.17/hspec.cabal" ""))

    it "knows a package's own cabal file from one further down" $
      fmap
        cabalFileAtTop
        [ "hspec-2.11.17/hspec.cabal",
          "hspec-2.11.17/vendor/other.cabal",
          "hspec.cabal"
        ]
        `shouldBe` [True, False, False]

    it "reads only a path written with the separator a tar file uses" $
      cabalFileAtTop "hspec-2.11.17\\hspec.cabal" `shouldBe` False

    it "takes the modules out of an archive's cabal file" $
      cabalFileInArchive
        ( archiveOf
            [ ("hspec-2.11.17/Setup.lhs", "main = undefined\n"),
              ( "hspec-2.11.17/hspec.cabal",
                header
                  <> "library\n\
                     \  exposed-modules: Test.Hspec, Test.Hspec.Runner\n"
              )
            ]
        )
        `shouldSatisfy` maybe False (elem "Test.Hspec.Runner" . modulesOf)

  describe "a tarball that cannot be read" $ do
    it "leaves its package unread when it is not gzip" $
      withTarball "not really a tarball" $ \path ->
        packageModules path `shouldReturn` Nothing

    it "leaves its package unread when it is cut short in its cabal file" $ do
      let names = ["M" <> T.pack (show n) | n <- [1 .. 5000 :: Int]]
          cabal =
            header <> "library\n  exposed-modules: " <> T.intercalate ", " names
          whole = GZip.compress (Tar.write [entryFor "big-1.0/big.cabal" cabal])
      withTarball (BL.take (BL.length whole `div` 2) whole) $ \path ->
        packageModules path `shouldReturn` Nothing

  describe "plain fields" $ do
    it "reads a one-line field" $
      exposed "library\n  exposed-modules: A.B, C.D\n"
        `shouldMatchList` ["A.B", "C.D"]

    it "reads a field spread over indented lines" $
      exposed "library\n  exposed-modules:\n    A.B\n    C.D\n    E\n"
        `shouldMatchList` ["A.B", "C.D", "E"]

    it "reads a mixture of commas and lines" $
      exposed "library\n  exposed-modules: A.B,\n    C.D\n"
        `shouldMatchList` ["A.B", "C.D"]

    it "is not confused by the field name's case" $
      exposed "library\n  Exposed-Modules: A.B\n" `shouldMatchList` ["A.B"]

    it "finds nothing when there is no such field" $
      exposed "library\n  build-depends: base\n" `shouldBe` []

  describe "what must not be picked up" $ do
    it "takes other-modules too, which a re-export may lead into" $
      exposed "library\n  exposed-modules: A\n  other-modules: B\n"
        `shouldMatchList` ["A", "B"]

    it "ignores reexported-modules" $
      exposed "library\n  exposed-modules: A\n  reexported-modules: B\n"
        `shouldMatchList` ["A"]

    it "stops at the next field" $
      exposed "library\n  exposed-modules:\n    A\n  build-depends: base\n"
        `shouldMatchList` ["A"]

  describe "conditionals" $ do
    it "takes a branch nested inside an if" $
      exposed
        "flag fancy\n\
        \library\n\
        \  exposed-modules: A\n\
        \  if flag(fancy)\n\
        \    exposed-modules: B\n"
        `shouldMatchList` ["A", "B"]

    it "takes both branches of an if/else" $
      exposed
        "library\n\
        \  if os(windows)\n\
        \    exposed-modules: W\n\
        \  else\n\
        \    exposed-modules: U\n"
        `shouldMatchList` ["W", "U"]

    it "takes a branch nested two deep" $
      exposed
        "flag a\n\
        \flag b\n\
        \library\n\
        \  if flag(a)\n\
        \    if flag(b)\n\
        \      exposed-modules: Deep\n"
        `shouldMatchList` ["Deep"]

    it "takes every library stanza, including named ones" $
      exposed
        "library\n\
        \  exposed-modules: Main.Lib\n\
        \\n\
        \library internal\n\
        \  exposed-modules: Internal.Lib\n"
        `shouldMatchList` ["Main.Lib", "Internal.Lib"]

  describe "comments" $ do
    it "does not let one at the margin cut a module list short" $
      exposed "library\n  exposed-modules:\n    A\n--    B\n    C\n"
        `shouldMatchList` ["A", "C"]

    it "does not count a module somebody commented out" $
      exposed "library\n  exposed-modules:\n    A\n    -- B\n    C\n"
        `shouldMatchList` ["A", "C"]

    it "keeps reading source directories past one" $
      sourceDirs
        "library\n\
        \  hs-source-dirs: src\n\
        \-- a comment\n\
        \test-suite t\n\
        \  type: exitcode-stdio-1.0\n\
        \  main-is: T.hs\n\
        \  hs-source-dirs: tests\n"
        `shouldBe` ["src", "tests", "."]

  describe "where a component with no hs-source-dirs lives" $ do
    it "offers the package directory even when other components name one" $
      sourceDirs
        "library\n\
        \  build-depends: base\n\
        \test-suite t\n\
        \  type: exitcode-stdio-1.0\n\
        \  main-is: T.hs\n\
        \  hs-source-dirs: tests\n"
        `shouldBe` ["tests", "."]

    it "offers it last, so a named directory is tried first" $
      last (sourceDirs "library\n  hs-source-dirs: src\n") `shouldBe` "."

    it "offers it once when it is named as well" $
      sourceDirs "library\n  hs-source-dirs: .\n" `shouldBe` ["."]

    it "offers it when nothing names anything" $
      sourceDirs "library\n  build-depends: base\n" `shouldBe` ["."]

  describe "a module's file in an archive" $ do
    it "is found under a source directory" $
      textOf "src" "Foo.Parser" [("t-0/src/Foo/Parser.hs", "found")]
        `shouldBe` Just "found"

    it "is found in the package directory" $
      textOf "." "Parser" [("t-0/Parser.hs", "found")]
        `shouldBe` Just "found"

    it "is found under a source directory however it is spelled" $
      fmap
        (\dir -> textOf dir "Parser" [("t-0/src/Parser.hs", "found")])
        ["src/", "./src"]
        `shouldBe` [Just "found", Just "found"]

    it "is not a file of that name below a source directory" $
      textOf "src" "Parser" [("t-0/src/Foo/Parser.hs", "module Foo.Parser")]
        `shouldBe` Nothing

    it "is not a file under a directory of that name further down" $
      textOf "src" "Parser" [("t-0/vendor/src/Parser.hs", "vendored")]
        `shouldBe` Nothing

  describe "what the package puts in force" $ do
    it "reads an extension the .cabal turns on" $
      extensions "library\n  default-extensions: LambdaCase\n"
        `shouldSatisfy` elem LambdaCase

    it "reads several, however they are written" $ do
      let found = extensions "library\n  default-extensions:\n    LambdaCase\n    MultiWayIf, BlockArguments\n"
      found `shouldSatisfy` elem LambdaCase
      found `shouldSatisfy` elem MultiWayIf
      found `shouldSatisfy` elem BlockArguments

    it "reads the extensions field older packages use" $
      extensions "library\n  extensions: BangPatterns\n"
        `shouldSatisfy` elem BangPatterns

    it "lets default-extensions take back what that field turns on" $
      extensions
        "library\n\
        \  extensions: LambdaCase\n\
        \  default-extensions: NoLambdaCase\n"
        `shouldSatisfy` notElem LambdaCase

    it "takes one back that the .cabal turns off" $
      extensions "library\n  default-extensions: ImplicitPrelude, NoImplicitPrelude\n"
        `shouldSatisfy` notElem ImplicitPrelude

    it "starts from what the language edition puts in force" $ do
      extensions "library\n  default-language: GHC2021\n"
        `shouldSatisfy` elem TypeOperators
      extensions "library\n  default-language: Haskell2010\n"
        `shouldSatisfy` notElem TypeOperators

    it "starts from Haskell98 where it names no edition, as cabal builds it" $ do
      extensions "library\n  build-depends: base\n"
        `shouldSatisfy` elem NondecreasingIndentation
      extensions "library\n  build-depends: base\n"
        `shouldSatisfy` notElem ImportQualifiedPost

    it "takes what every edition in the file puts in force" $
      extensions
        "library\n\
        \  default-language: Haskell2010\n\
        \test-suite spec\n\
        \  type: exitcode-stdio-1.0\n\
        \  main-is: T.hs\n\
        \  default-language: GHC2021\n"
        `shouldSatisfy` elem TypeOperators

    it "still has the earlier edition's own extensions" $
      extensions
        "library\n\
        \  default-language: Haskell2010\n\
        \test-suite spec\n\
        \  type: exitcode-stdio-1.0\n\
        \  main-is: T.hs\n\
        \  default-language: GHC2021\n"
        `shouldSatisfy` elem ImplicitPrelude

    it "passes over a name no compiler knows" $
      extensions "library\n  default-extensions: LambdaCase, NotAnExtension\n"
        `shouldSatisfy` elem LambdaCase

    it "takes every component's, since it does not know which one asks" $ do
      let found =
            extensions
              "library\n\
              \  default-extensions: LambdaCase\n\
              \test-suite t\n\
              \  type: exitcode-stdio-1.0\n\
              \  main-is: T.hs\n\
              \  default-extensions: MultiWayIf\n"
      found `shouldSatisfy` elem LambdaCase
      found `shouldSatisfy` elem MultiWayIf

  describe "the union is deliberate" $
    it "does not need to know which branch a build would take" $ do
      let both =
            exposed
              "library\n\
              \  if impl(ghc >= 9.6)\n\
              \    exposed-modules: New\n\
              \  else\n\
              \    exposed-modules: Old\n"
      both `shouldMatchList` ["New", "Old"]

-- | The modules a @.cabal@ of this shape holds.
exposed :: Text -> [Text]
exposed = modulesOf . described

-- | The directories a @.cabal@ of this shape offers modules under.
sourceDirs :: Text -> [FilePath]
sourceDirs = packageSourceDirs . described

-- | What a @.cabal@ of this shape puts in force.
extensions :: Text -> [Extension]
extensions = (`effectiveExtensions` "") . packageExtensions . described

-- | The text an archive of these files gives for a module, where its
-- @.cabal@ file names this source directory.
textOf :: FilePath -> Text -> [(FilePath, BS.ByteString)] -> Maybe Text
textOf dir modName files =
  case moduleIn modName (Archive (Just cabal) files) of
    Just (Haskell text) -> Just text
    _ -> Nothing
  where
    cabal =
      described ("library\n  hs-source-dirs: " <> T.pack dir <> "\n")

-- | What a @.cabal@ file holding the fields given describes.
described :: Text -> GenericPackageDescription
described fields =
  either (error . T.unpack . describePackageProblem) id $
    parsePackageDescription "t.cabal" (T.encodeUtf8 (header <> fields))

-- | The fields a @.cabal@ file needs before its components.
header :: Text
header = "cabal-version: 2.4\nname: t\nversion: 0\n"

-- | The names of the modules a package declares.
modulesOf :: GenericPackageDescription -> [Text]
modulesOf = fmap fst . concatMap snd . declaredModules . packageBranches

-- | A tar entry at the given path, holding the given text.
entryFor :: FilePath -> Text -> Tar.Entry
entryFor path contents = case Tar.toTarPath False path of
  Left why -> error why
  Right tarPath -> Tar.fileEntry tarPath (BL.fromStrict (T.encodeUtf8 contents))

-- | Run something with a tarball holding these bytes.
withTarball :: BL.ByteString -> (FilePath -> Expectation) -> Expectation
withTarball bytes act =
  withSystemTempDirectory "tilia-tarball" $ \dir -> do
    let path = dir </> "big-1.0.tar.gz"
    BL.writeFile path bytes
    act path

-- | An archive of those entries, in order.
archiveOf :: [(FilePath, Text)] -> Tar.Entries e
archiveOf = foldr (Tar.Next . uncurry entryFor) Tar.Done
