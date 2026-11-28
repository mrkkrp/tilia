{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Properties that should hold of every module, wherever comments are
-- written in it.
module Tilia.Comments.PropertiesSpec (spec) where

import Control.Monad (filterM)
import Data.Choice (pattern Do)
import Data.Either (isRight)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Generics.Schemes (listify)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Types.SrcLoc qualified as GHC
import System.FilePath (takeFileName)
import Test.Hspec hiding (before)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

import Tilia.Corpus (Example (..), obtain, vendoredExamples)
import Tilia.Format (describeFormatError, formattingFailed)
import Tilia.Palette (Palette (Plain))
import Tilia.Parser (parseModule, pmModule)
import Tilia.Settings (defaultSettings)
import Tilia.TestConfig (formatExample)
import Tilia.TestUtils (defaultParserConfig, withEachSettings)
import Tilia.Utils (readUtf8File, tshow)

spec :: Spec
spec = do
  vendored <- runIO (either (fail . T.unpack) pure =<< obtain vendoredExamples)
  modules <-
    runIO $
      filterM formatsAsWritten
        =<< traverse withPlaces (fmap exampleInput vendored)
  parallel . modifyMaxSuccess (const 5) . withEachSettings $ \settings ->
    describe "formats commented anywhere in one pass, as the same program" $
      for_ [m | m@(_, _, _ : _) <- modules] $ \(path, source, places) ->
        it path . forAllShrink (placesAmong places) shrinkPlaces $ \chosen ->
          let written = commented chosen source
           in ioProperty $
                formatExample
                  (Do #check)
                  settings
                  Nothing
                  Map.empty
                  (takeFileName path)
                  written
                  <&> \case
                    Left e
                      | formattingFailed e ->
                          counterexample
                            ( T.unpack
                                (written <> "\n" <> describeFormatError Plain e)
                            )
                            False
                      | otherwise -> property Discard
                    Right _ -> property True

-- | Does a module format with no comment written in it, so that what
-- writing comments in it does can be told?
formatsAsWritten ::
  -- | The module, read, with the places a comment can be written in it.
  (FilePath, Text, [Place]) ->
  IO Bool
formatsAsWritten (path, source, _) =
  isRight
    <$> formatExample
      (Do #check)
      defaultSettings
      Nothing
      Map.empty
      (takeFileName path)
      source

-- | Where a comment can be written: after a token, ending its line there,
-- or on a line of its own above a line that a token begins.
data Place
  = -- | After the token that ends at this line and column.
    AfterToken !Int !Int
  | -- | Above this line.
    AboveLine !Int
  deriving (Eq, Ord, Show)

-- | A module read from a file, with the places a comment can be written
-- in it.
withPlaces ::
  -- | The file.
  FilePath ->
  IO (FilePath, Text, [Place])
withPlaces path = do
  source <- fromMaybe "" <$> readUtf8File path
  pure (path, source, placesIn path source)

-- | The places a comment can be written in a module: after every token and
-- above every line a token begins, except on a line holding a tab or a
-- preprocessor directive.
placesIn ::
  -- | Its path.
  FilePath ->
  -- | The module.
  Text ->
  [Place]
placesIn path source =
  case parseModule defaultParserConfig (takeFileName path) source of
    Left _ -> []
    Right m ->
      Set.toList . Set.fromList $
        concatMap placesOf (listify (const True) (pmModule m))
  where
    ls = T.lines source
    placesOf s =
      [ AfterToken (GHC.srcSpanEndLine s) (GHC.srcSpanEndCol s)
      | plain (GHC.srcSpanEndLine s)
      ]
        <> [ AboveLine (GHC.srcSpanStartLine s)
           | plain (GHC.srcSpanStartLine s),
             beginsLine s
           ]
    beginsLine s = case drop (GHC.srcSpanStartLine s - 1) ls of
      t : _ -> T.length (T.takeWhile (== ' ') t) == GHC.srcSpanStartCol s - 1
      [] -> False
    plain n = case drop (n - 1) ls of
      t : _ ->
        not (T.any (== '\t') t) && not ("#" `T.isPrefixOf` T.stripStart t)
      [] -> False

-- | One to three of the places.
placesAmong ::
  -- | The places.
  [Place] ->
  Gen [Place]
placesAmong places = do
  n <- choose (1, 3)
  nub <$> vectorOf n (elements places)

-- | Fewer of the places.
shrinkPlaces ::
  -- | The places.
  [Place] ->
  [[Place]]
shrinkPlaces = shrinkList (const [])

-- | A module with a numbered comment written at each place, the places
-- taken from the last to the first so that each is still where it was.
--
-- What follows a comment written after a token goes on the next line at
-- the column it was at, so that the module stays the same program.
commented ::
  -- | The places.
  [Place] ->
  -- | The module.
  Text ->
  Text
commented places source =
  T.unlines . foldl write (T.lines source) $
    sortOn (Down . key . snd) (zip [1 :: Int ..] places)
  where
    key = \case
      AfterToken l c -> (l, c)
      AboveLine l -> (l, 0)
    write ls (n, place) = case place of
      AfterToken l c -> case splitAt (l - 1) ls of
        (above, t : below) ->
          let before = T.take (c - 1) t
              rest = T.drop (c - 1) t
              column = T.length before + T.length (T.takeWhile (== ' ') rest)
              moved = T.replicate column " " <> T.stripStart rest
           in above
                <> [T.stripEnd before <> " " <> comment n]
                <> [moved | not (T.null (T.strip rest))]
                <> below
        _ -> ls
      AboveLine l -> case splitAt (l - 1) ls of
        (above, t : below) ->
          above
            <> [T.takeWhile (== ' ') t <> comment n, t]
            <> below
        _ -> ls
    comment n = "-- c" <> tshow n
