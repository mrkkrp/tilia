{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Formatting what an editor holds of a module.
module Tilia.EditorSpec (spec) where

import Data.Choice (pattern Don't)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Editor (editorSession, formatBuffer)
import Tilia.Format (FormatError, formatErrorExitCode)
import Tilia.Run (Outcome (..))

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

  it "takes fixities from the modules the buffer imports" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") "module Uses where\nimport Ops\nx=a <+> b\n"
        `shouldReturn` CameTo "module Uses where\n\nimport Ops\n\nx = a <+> b\n"

  it "declines an operator from a module it cannot read" $
    withProject $ \buffer ->
      buffer ("src" </> "Uses.hs") "module Uses where\nimport Nowhere\nx=a <?> b\n"
        `shouldReturn` CameDeclined 15

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

-- | A project with a module declaring @infixl 6 <+>@, a @.tiliaignore@
-- excluding @src/Generated.hs@, and a plan for it trusted as up to date,
-- handed to the test as a way to format a buffer as one of its files.
withProject :: ((FilePath -> Text -> IO Came) -> Expectation) -> Expectation
withProject act =
  withSystemTempDirectory "tilia-editor" $ \dir -> do
    let project = dir </> "project"
        planFile = dir </> "elsewhere" </> "plan.json"
        write path text = do
          createDirectoryIfMissing True (takeDirectory path)
          T.writeFile path text
    write (project </> "fake.cabal") $
      T.unlines
        [ "cabal-version: 2.4",
          "name: fake",
          "version: 0.1.0.0",
          "library",
          "  exposed-modules: Ops",
          "  hs-source-dirs: src",
          "  default-language: Haskell2010"
        ]
    write (project </> "src" </> "Ops.hs") $
      T.unlines
        [ "module Ops ((<+>)) where",
          "infixl 6 <+>",
          "(<+>) :: a -> a -> a",
          "(<+>) = const"
        ]
    write (project </> ".tiliaignore") "src/Generated.hs\n"
    write planFile $
      "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":[\
      \{\"pkg-name\":\"fake\",\"pkg-version\":\"0.1.0.0\",\
      \\"pkg-src\":{\"type\":\"local\",\"path\":\"./.\"}}]}"
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
