{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Formatting at column limits other than the default one.
module Tilia.ColumnLimitSpec (spec) where

import Control.Monad (guard)
import Data.Choice (pattern Do)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath (dropExtension, takeFileName, (</>))
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Text.Read (readMaybe)

import Tilia.Corpus (Example (..), obtain, vendoredExamples)
import Tilia.Format (describeFormatError, formattingFailed)
import Tilia.Palette (Palette (Plain))
import Tilia.Settings (Settings (..), defaultSettings)
import Tilia.TestConfig (formatExample)
import Tilia.TestUtils (haskellFilesIn, withEachSettings)
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
  vendored <- runIO (either (fail . T.unpack) pure =<< obtain vendoredExamples)
  modules <-
    runIO $
      traverse
        (\f -> (f,) . fromMaybe "" <$> readUtf8File f)
        ( fmap exampleInput vendored
            <> [f | f <- files, Nothing <- [limitOf f]]
        )
  parallel . modifyMaxSuccess (const 5) . withEachSettings $ \settings ->
    describe "formats at any column limit in one pass, as the same program" $
      for_ modules $ \(path, source) ->
        it path . forAll (choose (1, widest source)) $ \limit ->
          ioProperty $
            formatExample
              (Do #check)
              settings{settingsColumnLimit = limit}
              Nothing
              Map.empty
              (takeFileName path)
              source
              <&> \case
                Left e
                  | formattingFailed e ->
                      counterexample
                        (T.unpack (describeFormatError Plain e))
                        False
                _ -> property True

-- | The width of the widest line of a module.
widest ::
  -- | The module.
  Text ->
  Int
widest = maximum . (1 :) . fmap T.length . T.lines

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
