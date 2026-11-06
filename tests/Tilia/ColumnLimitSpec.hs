{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Formatting at column limits other than the default one.
module Tilia.ColumnLimitSpec (spec) where

import Control.Monad (guard)
import Data.Choice (pattern Do)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import System.FilePath (dropExtension, takeFileName, (</>))
import Test.Hspec
import Text.Read (readMaybe)

import Tilia.Format (describeFormatError)
import Tilia.Palette (Palette (Plain))
import Tilia.Settings (Settings (..), defaultSettings)
import Tilia.TestConfig (formatExample)
import Tilia.TestUtils (haskellFilesIn)
import Tilia.Utils (readUtf8File)

spec :: Spec
spec = do
  files <- runIO (haskellFilesIn ("corpora" </> "column-limit"))
  describe "a module formatted at a column limit" $
    for_ [f | f <- files, Nothing <- [limitOf f]] $ \input ->
      for_
        [ (limit, f)
        | f <- files,
          Just limit <- [limitOf f],
          f == dropExtension input <> "-out-" <> show limit <> ".hs"
        ]
        $ \(limit, reference) ->
          it (takeFileName input <> " at " <> show limit) $ do
            Just source <- readUtf8File input
            Just expected <- readUtf8File reference
            formatExample
              (Do #check)
              defaultSettings{settingsColumnLimit = limit}
              Nothing
              Map.empty
              (takeFileName input)
              source
              >>= \case
                Left e ->
                  expectationFailure (T.unpack (describeFormatError Plain e))
                Right formatted -> formatted `shouldBe` expected

-- | The column limit a file of the collection says what a module formats
-- to at, as @name-out-N.hs@ does, if it is one of those.
limitOf ::
  -- | The file.
  FilePath ->
  Maybe Int
limitOf f = do
  let (front, n) = T.breakOnEnd "-out-" (T.pack (dropExtension f))
  guard (not (T.null front))
  readMaybe (T.unpack n)
