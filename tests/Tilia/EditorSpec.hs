{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Formatting what an editor holds of a module.
module Tilia.EditorSpec (spec) where

import Control.Exception (throwIO)
import Data.Choice (pattern Don't)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (makeVersion)
import GHC.LanguageExtensions.Type (Extension (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Tilia.Editor
  ( Compiled (..),
    compiledSession,
    editorSession,
    formatBuffer,
  )
import Tilia.Fixity (Direction (..), Fixity (..), Namespace (..), OpName (..))
import Tilia.Fixity.HiFile (HiExport (..), HiFile (..), HiName (..))
import Tilia.Format (FormatError, Session (..), formatErrorExitCode)
import Tilia.Run (Outcome (..))
import Tilia.TestUtils (withOpsProject, withStandIn)
import Tilia.Utils (writeUtf8File)

spec :: Spec
spec = describe "formatting what an editor holds" $ do
  it "formats the buffer rather than the file it comes from" $
    withProject $ \buffer ->
      buffer ("src" </> "Ops.hs") "module Ops where\nx=1\n"
        `shouldReturn` CameTo "module Ops where\n\nx = 1\n"

  it "says nothing changed where nothing would" $
    withProject $ \buffer ->
      buffer ("src" </> "Ops.hs") "module Ops where\n\nx = 1\n"
        `shouldReturn` CameOut

  it "formats a module that is not on disk yet" $
    withProject $ \buffer ->
      buffer ("src" </> "New.hs") "module New where\nx=1\n"
        `shouldReturn` CameTo "module New where\n\nx = 1\n"

  it "keeps the line endings of the buffer" $
    withProject $ \buffer ->
      buffer ("src" </> "Ops.hs") "module Ops where\r\nx=1\r\n"
        `shouldReturn` CameTo "module Ops where\r\n\r\nx = 1\r\n"

  it "keeps the byte order mark the buffer begins with" $
    withProject $ \buffer ->
      buffer ("src" </> "Ops.hs") "\xFEFFmodule Ops where\nx=1\n"
        `shouldReturn` CameTo "\xFEFFmodule Ops where\n\nx = 1\n"

  it "takes fixities from the modules the buffer imports" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") "module Uses where\nimport Ops\nx=a <+> b\n"
        `shouldReturn` CameTo "module Uses where\n\nimport Ops\n\nx = a <+> b\n"

  it "puts the imports of the project's own modules after the rest" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") importsOfBoth
        `shouldReturn` CameTo importsOfBothGrouped

  it "takes an import naming another package for one of that package's" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") importsByPackage
        `shouldReturn` CameTo importsByPackageGrouped

  it "takes fixities from a module that begins with a byte order mark" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") "module Uses where\nimport Marked\nx=a <-> b\n"
        `shouldReturn` CameTo
          "module Uses where\n\nimport Marked\n\nx = a <-> b\n"

  it "declines an operator from a module it cannot read" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") "module Uses where\nimport Nowhere\nx=a <?> b\n"
        `shouldReturn` CameDeclined 18

  it "takes fixities for a buffer whose branches do not parse together" $
    withProject $ \buffer ->
      buffer ("src" </> "Checks.hs") alternatives `shouldReturn` CameOut

  it "declines an operator it cannot settle where the branches do not parse together" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") alternativesUsingUnknown
        `shouldReturn` CameDeclined 18

  it "leaves alone a file the project's .tiliaignore excludes" $
    withProject $ \buffer ->
      buffer ("src" </> "Generated.hs") "module Generated where\nx=1\n"
        `shouldReturn` CameOut

  it "fails on a buffer that does not parse" $
    withProject $ \buffer ->
      buffer ("src" </> "Ops.hs") "module Ops where\nx = (\n"
        `shouldReturn` CameFailed 4

  it "fails where no project is above the file" $
    withSystemTempDirectory "tilia-editor" $ \dir ->
      either (Just . failure) (const Nothing)
        <$> editorSession
          (dir </> "A.hs")
          Nothing
          (Don't #useCache)
          (Don't #download)
          (Don't #checkAst)
          (Don't #checkIdempotence)
          (Don't #debugFixity)
        `shouldReturn` Just 2

  it "fails where a .cabal file of the project does not parse" $
    inProject $ \project planFile -> do
      writeUtf8File (project </> "fake.cabal") "library\n  exposed-modules: {\n"
      either (Just . failure) (const Nothing)
        <$> editorSession
          (project </> "src" </> "Ops.hs")
          (Just planFile)
          (Don't #useCache)
          (Don't #download)
          (Don't #checkAst)
          (Don't #checkIdempotence)
          (Don't #debugFixity)
        `shouldReturn` Just 25

  describe "where Cabal cannot solve a plan" $ do
    it "says why it went on without one" $
      withUnsolved $ \session _ ->
        sessionUnsolved session `shouldSatisfy` isJust

    it "takes fixities from the project's own modules" $
      withUnsolved $ \_ buffer ->
        buffer ("src" </> "Uses.hs") "module Uses where\nimport Ops\nx=a <+> b\n"
          `shouldReturn` CameTo
            "module Uses where\n\nimport Ops\n\nx = a <+> b\n"

    it "takes fixities from the compiler's own packages" $
      withUnsolved $ \_ buffer ->
        buffer ("src" </> "Uses.hs") "module Uses where\nx=f $ g . h\n"
          `shouldReturn` CameTo "module Uses where\n\nx = f $ g . h\n"

    it "declines an operator only a plan would say where to read" $
      withUnsolved $ \_ buffer ->
        buffer ("src" </> "Uses.hs") "module Uses where\nimport Nowhere\nx=a <?> b\n"
          `shouldReturn` CameDeclined 18

    it "leaves open the branches of another package's version" $
      withUnsolved $ \_ buffer ->
        buffer ("src" </> "Uses.hs") versionedElsewhere
          `shouldReturn` CameDeclined 18

  describe "given what compiling the project establishes" $ do
    it "puts the imports of the project's own modules after the rest" $
      withCompiled $ \buffer ->
        buffer ("src" </> "Uses.hs") importsOfBoth
          `shouldReturn` CameTo importsOfBothGrouped

    it "takes fixities from the interfaces it is given" $
      withCompiled $ \buffer ->
        buffer ("src" </> "Uses.hs") "module Uses where\nimport Ops\nx=a <+> b\n"
          `shouldReturn` CameTo
            "module Uses where\n\nimport Ops\n\nx = a <+> b\n"

    it "declines an operator from a module it is given no interface of" $
      withCompiled $ \buffer ->
        buffer ("src" </> "Uses.hs") "module Uses where\nimport Nowhere\nx=a <?> b\n"
          `shouldReturn` CameDeclined 18

    it "parses with the extensions it is given rather than the package's" $
      withCompiled $ \buffer ->
        buffer ("src" </> "Uses.hs") "module Uses where\nf = \\case\n    _ -> 1\n"
          `shouldReturn` CameTo "module Uses where\n\nf = \\case\n  _ -> 1\n"

    it "fails a module whose formatting throws rather than throwing itself" $
      withCompiled $ \buffer ->
        buffer ("src" </> "Uses.hs") "module Uses where\nimport Boom\nx=a <?> b\n"
          `shouldReturn` CameFailed 22

    it "leaves out the branches the versions it is given rule out" $
      withCompiled $ \buffer ->
        buffer ("src" </> "Uses.hs") versioned `shouldReturn` CameOut

-- | A module importing one of the project's own modules among others, with
-- a comment between them.
importsOfBoth :: Text
importsOfBoth =
  T.unlines
    [ "module Uses where",
      "import Ops",
      "",
      "-- Sorting.",
      "import Data.List (sort)",
      "import Data.Char",
      "x = sort"
    ]

-- | 'importsOfBoth', with the project's own modules after the rest and no
-- empty line in either group.
importsOfBothGrouped :: Text
importsOfBothGrouped =
  T.unlines
    [ "module Uses where",
      "",
      "import Data.Char",
      "-- Sorting.",
      "import Data.List (sort)",
      "",
      "import Ops",
      "",
      "x = sort"
    ]

-- | A module importing the project's own module and a module of the same
-- name from another package, each by the name of its package.
importsByPackage :: Text
importsByPackage =
  T.unlines
    [ "{-# LANGUAGE PackageImports #-}",
      "module Uses where",
      "import \"fake\" Ops",
      "import qualified \"other\" Ops as Other",
      "import Data.List (sort)",
      "x = sort"
    ]

-- | 'importsByPackage', with the import naming the project's package after
-- the rest.
importsByPackageGrouped :: Text
importsByPackageGrouped =
  T.unlines
    [ "{-# LANGUAGE PackageImports #-}",
      "",
      "module Uses where",
      "",
      "import Data.List (sort)",
      "import qualified \"other\" Ops as Other",
      "",
      "import \"fake\" Ops",
      "",
      "x = sort"
    ]

-- | A formatted module with a conditional around two alternatives, which do
-- not parse one after the other.
alternatives :: Text
alternatives =
  T.unlines
    [ "{-# LANGUAGE CPP #-}",
      "",
      "module Checks where",
      "",
      "g :: IO ()",
      "g =",
      "#ifdef CHECK",
      "  id $",
      "    do",
      "      print 1",
      "#else",
      "  do",
      "    print 1",
      "#endif",
      "",
      "h :: Int -> String",
      "h n =",
      "  error $",
      "    \"count: \" ++ show n"
    ]

-- | The same kind of alternatives, using an operator from a module that
-- cannot be read.
alternativesUsingUnknown :: Text
alternativesUsingUnknown =
  T.unlines
    [ "{-# LANGUAGE CPP #-}",
      "module Uses where",
      "import Nowhere",
      "x =",
      "#ifdef CHECK",
      "  id $ do",
      "    a <?> b",
      "#else",
      "  do",
      "    a <?> b",
      "#endif"
    ]

-- | A module with a branch for a version of the package other than the one
-- built, which uses an operator from a module only that version has.
versioned :: Text
versioned =
  T.unlines
    [ "{-# LANGUAGE CPP #-}",
      "",
      "module Uses where",
      "",
      "#if MIN_VERSION_fake(1,0,0)",
      "import Nowhere",
      "",
      "x = a <?> b",
      "#else",
      "import Ops",
      "",
      "x = a <+> b",
      "#endif"
    ]

-- | 'versioned', asking about the version of a package no plan names.
versionedElsewhere :: Text
versionedElsewhere = T.replace "MIN_VERSION_fake" "MIN_VERSION_other" versioned

-- | What formatting a buffer came to, with errors told apart by the status
-- they exit with.
data Came
  = -- | Nothing to change.
    CameOut
  | -- | Formatted into this.
    CameTo Text
  | -- | Declined.
    CameDeclined Int
  | -- | Failed.
    CameFailed Int
  deriving (Eq, Show)

-- | Tell what became of a buffer.
came :: Outcome -> Came
came = \case
  Unchanged -> CameOut
  Changed _ formatted -> CameTo formatted
  Declined e -> CameDeclined (failure e)
  Failed e -> CameFailed (failure e)

-- | The status a failure exits with.
failure :: FormatError -> Int
failure = formatErrorExitCode

-- | A project with a module declaring @infixl 6 <+>@, one beginning with a
-- byte order mark declaring @infixr 0 <->@, a @.tiliaignore@
-- excluding @src/Generated.hs@, and a plan for it trusted as up to date,
-- handed to the test as a way to format a buffer as one of its files.
withProject :: ((FilePath -> Text -> IO Came) -> Expectation) -> Expectation
withProject act =
  inProject $ \project planFile ->
    act $ \file text -> do
      let path = project </> file
      editorSession
        path
        (Just planFile)
        (Don't #useCache)
        (Don't #download)
        (Don't #checkAst)
        (Don't #checkIdempotence)
        (Don't #debugFixity)
        >>= \case
          Left e -> pure (CameFailed (failure e))
          Right session -> came <$> formatBuffer session path text

-- | The project of 'withProject' with no plan, and a stand-in for @cabal@
-- that cannot solve one, handed to the test as the session made for it
-- and a way to format a buffer as one of its files.
withUnsolved ::
  (Session -> (FilePath -> Text -> IO Came) -> Expectation) -> Expectation
withUnsolved act =
  inProject $ \project _ -> do
    (_, made) <-
      withStandIn "cabal" "exit 1" $
        editorSession
          (project </> "src" </> "Ops.hs")
          Nothing
          (Don't #useCache)
          (Don't #download)
          (Don't #checkAst)
          (Don't #checkIdempotence)
          (Don't #debugFixity)
    case made of
      Left e -> expectationFailure ("came to " <> show (failure e))
      Right session ->
        act session $ \file text ->
          came <$> formatBuffer session (project </> file) text

-- | The project of 'withProject', with what compiling it would establish
-- given instead of a plan: @LambdaCase@ in force, version 0.1.0.0 of the
-- package, and the interfaces of @Ops@ and of an empty @Prelude@; asking for
-- that of @Boom@ throws.
withCompiled :: ((FilePath -> Text -> IO Came) -> Expectation) -> Expectation
withCompiled act =
  inProject $ \project _ ->
    act $ \file text -> do
      let path = project </> file
      compiledSession path compiled (Don't #checkAst) (Don't #checkIdempotence)
        >>= \case
          Left e -> pure (CameFailed (failure e))
          Right session -> came <$> formatBuffer session path text
  where
    compiled =
      Compiled
        { compiledExtensions = [ImplicitPrelude, LambdaCase],
          compiledWith = makeVersion [9, 14, 1],
          compiledPackages = Map.singleton "fake" (makeVersion [0, 1, 0, 0]),
          compiledInterface = \case
            "Boom" -> throwIO (userError "boom")
            m -> pure (Map.lookup m interfaces)
        }
    interfaces =
      Map.fromList
        [ ( "Ops",
            HiFile
              { hiModule = "Ops",
                hiExports = [Avail (HiName "Ops" InTerms (OpName "<+>"))],
                hiFixities = [(InTerms, OpName "<+>", Fixity LeftAssoc 6)]
              }
          ),
          ("Prelude", HiFile{hiModule = "Prelude", hiExports = [], hiFixities = []})
        ]

-- | A project with a module declaring @infixl 6 <+>@, one beginning with a
-- byte order mark declaring @infixr 0 <->@, and a @.tiliaignore@ excluding
-- @src/Generated.hs@, given to the test with a plan for it.
inProject :: (FilePath -> FilePath -> Expectation) -> Expectation
inProject =
  withOpsProject
    [ ( "src" </> "Marked.hs",
        T.unlines
          [ "\xFEFFmodule Marked ((<->)) where",
            "infixr 0 <->",
            "(<->) :: a -> a -> a",
            "(<->) = const"
          ]
      ),
      (".tiliaignore", "src/Generated.hs\n")
    ]
    []
