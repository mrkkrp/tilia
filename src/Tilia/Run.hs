{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Running the formatter over a set of files.
module Tilia.Run
  ( -- * Outcomes
    Outcome (..),
    declined,
    failed,
    differs,
    exitCodeOf,

    -- * Execution
    runOver,
    readAsUtf8,
    formattingOutcome,
    writeBack,
    inParallel,

    -- * Report
    Report (..),
    inplaceReport,
    checkReport,
    noted,
  )
where

import Control.Concurrent
  ( forkIO,
    getNumCapabilities,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Monad (replicateM)
import Data.ByteString qualified as BS
import Data.Foldable (for_, traverse_)
import Data.IORef
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.FilePath (takeExtension)
import Tilia.Diff (diffInFull)
import Tilia.Format
  ( FormatError (Unreadable),
    Session,
    describeFormatError,
    formatErrorExitCode,
    formatSource,
    refused,
  )
import Tilia.Newline (NewlineStyle (Lf), getNewlineStyle, setNewlineStyle)
import Tilia.Palette (Color (Bad, Good, Middling, Place), Palette, marker, paint)
import Tilia.Utils (attempted, indent, lineWidth, wrapTo)

----------------------------------------------------------------------------
-- Outcomes

-- | What became of one file.
data Outcome
  = -- | The file did not need to change.
    Unchanged
  | -- | The file got formatted, the arguments are texts before and after.
    Changed Text Text
  | -- | Declined.
    Declined FormatError
  | -- | Failed to format.
    Failed FormatError

-- | Was the file declined?
declined :: Outcome -> Bool
declined = \case
  Declined{} -> True
  _ -> False

-- | Did the file fail to format?
failed :: Outcome -> Bool
failed = \case
  Failed{} -> True
  _ -> False

-- | Would formatting change the file?
differs :: Outcome -> Bool
differs = \case
  Changed{} -> True
  _ -> False

-- | Determine the exit code based on the set of outcomes. 'Nothing' means
-- success.
exitCodeOf :: [(FilePath, Outcome)] -> Maybe Int
exitCodeOf outcomes =
  case [formatErrorExitCode e | (_, Failed e) <- outcomes] of
    [] -> Nothing
    codes -> Just (minimum codes)

----------------------------------------------------------------------------
-- Execution

-- | Format every file, as many at a time as the machine allows.
runOver :: Session -> [FilePath] -> IO [(FilePath, Outcome)]
runOver session = inParallel one
  where
    one path = do
      !outcome <-
        readAsUtf8 path >>= \case
          Left why -> pure (Failed (Unreadable path why))
          Right before ->
            formatSource session path (setNewlineStyle Lf before) >>= \case
              Left e -> pure (if refused e then Declined e else Failed e)
              Right formatted -> pure (formattingOutcome before formatted)
      pure (path, outcome)

-- | Read a source file as UTF-8.
readAsUtf8 :: FilePath -> IO (Either Text Text)
readAsUtf8 path =
  attempted (BS.readFile path) >>= \case
    Left why -> pure (Left why)
    Right bytes -> pure $ case T.decodeUtf8' bytes of
      Right text -> Right text
      Left _ -> Left "it is not valid UTF-8"

-- | Formatting outcome for a file.
formattingOutcome ::
  -- | The file, as it is.
  Text ->
  -- | Its formatted text, in newlines.
  Text ->
  Outcome
formattingOutcome before formatted
  | after == before = Unchanged
  | otherwise = Changed before after
  where
    after = setNewlineStyle (getNewlineStyle before) formatted

-- | Put a formatted file back, and only if it changed.
writeBack :: (FilePath, Outcome) -> IO ()
writeBack (path, outcome) = case outcome of
  Changed _ after -> BS.writeFile path (T.encodeUtf8 after)
  _ -> pure ()

-- | Run an action over every element at once, as far as the machine allows.
inParallel :: (a -> IO b) -> [a] -> IO [b]
inParallel act xs = do
  capabilities <- getNumCapabilities
  queue <- newIORef (zip [0 :: Int ..] xs)
  answers <- newIORef Map.empty
  let worker =
        atomicModifyIORef'
          queue
          (\case [] -> ([], Nothing); (y : ys) -> (ys, Just y))
          >>= \case
            Nothing -> pure ()
            Just (i, x) -> do
              y <- act x
              atomicModifyIORef' answers (\m -> (Map.insert i y m, ()))
              worker
  done <- replicateM (max 1 (min capabilities (length xs))) newEmptyMVar
  for_ done $ \signal -> forkIO (worker >> putMVar signal ())
  traverse_ takeMVar done
  Map.elems <$> readIORef answers

----------------------------------------------------------------------------
-- Report

-- | What to print when a run is over, and on which stream.
data Report = Report
  { -- | For standard output.
    reportOut :: [Text],
    -- | For standard error.
    reportErr :: [Text]
  }
  deriving (Eq, Show)

-- | The summary an @inplace@ run prints.
inplaceReport :: Palette -> [(FilePath, Outcome)] -> Report
inplaceReport palette outcomes =
  Report
    { reportOut = tally palette ("✓", Good) "Formatted" (not . skipped) outcomes,
      reportErr = asides palette outcomes
    }
  where
    skipped o = declined o || failed o

-- | The diffs a @check@ run prints, and what it could not or would not do.
checkReport :: Palette -> [(FilePath, Outcome)] -> Report
checkReport palette outcomes =
  Report
    { reportOut =
        [ diffInFull palette path before after
        | (path, Changed before after) <- outcomes
        ],
      reportErr = asides palette outcomes
    }

-- | Everything said about the files that were not formatted.
asides :: Palette -> [(FilePath, Outcome)] -> [Text]
asides palette outcomes =
  concat
    [ tally palette ("=", Middling) "Declined" declined outcomes,
      reasons declined,
      tally palette ("✗", Bad) "Failed" failed outcomes,
      reasons failed
    ]
  where
    reasons wanted =
      [ line
      | (_, outcome) <- sortOn fst outcomes,
        wanted outcome,
        e <- why outcome,
        line <- bulleted palette e
      ]
    why = \case
      Declined e -> [e]
      Failed e -> [e]
      _ -> []

-- | One line per extension, for the files a test picks out.
tally ::
  Palette ->
  -- | The mark to set the line under, and the color to set it in.
  (Text, Color) ->
  -- | What became of the files being counted.
  Text ->
  -- | Which outcomes to count.
  (Outcome -> Bool) ->
  -- | Every file of the run, and what became of it.
  [(FilePath, Outcome)] ->
  [Text]
tally palette (mark, color) what wanted outcomes =
  [ indent 1
      <> marker palette color mark
      <> " "
      <> what
      <> " "
      <> count palette n extension
  | (extension, n) <- countedBy (wanted . snd) outcomes
  ]

-- | How many files of each extension, among the ones a test picks out.
countedBy ::
  -- | Which files to count.
  ((FilePath, Outcome) -> Bool) ->
  -- | Every file of the run, and what became of it.
  [(FilePath, Outcome)] ->
  [(Text, Int)]
countedBy wanted =
  Map.toList
    . Map.fromListWith (+)
    . fmap (\(path, _) -> (T.pack (takeExtension path), 1 :: Int))
    . filter wanted

-- | Render the number of files.
count :: Palette -> Int -> Text -> Text
count palette n extension =
  T.pack (show n)
    <> " "
    <> paint palette Place extension
    <> (if n == 1 then " file" else " files")

-- | One case among several, opened by a bullet and wrapped underneath it.
bulleted :: Palette -> FormatError -> [Text]
bulleted palette e = case wrapTo (lineWidth - 6) (describeFormatError palette e) of
  [] -> []
  (opening : rest) ->
    (indent 2 <> "· " <> opening) : fmap (indent 3 <>) rest

-- | Something to say under a mark of its own, wrapped to fit beneath it.
noted ::
  Palette ->
  -- | The mark to set it under, and the color to set that in.
  (Text, Color) ->
  Text ->
  [Text]
noted palette (mark, color) text = case wrapTo (lineWidth - 6) text of
  [] -> []
  (opening : rest) ->
    (indent 1 <> marker palette color mark <> " " <> opening)
      : fmap (indent 3 <>) rest
