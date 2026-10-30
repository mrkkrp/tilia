{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Miscellaneous utilities.
module Tilia.Utils
  ( quietly,
    attempted,
    readUtf8File,
    writeUtf8File,
    fieldWords,
    blocksOf,
    indented,
    looksLikeModule,
    pathUnder,
    asUtf8,
    byteOrderMark,
    sha256Of,
    sha256OfFile,
    lineWidth,
    indent,
    wrapTo,
    visibleLength,
    spellList,
    collected,
    tshow,
    inParallel,
  )
where

import Control.Concurrent
  ( forkFinally,
    getNumCapabilities,
    newEmptyMVar,
    putMVar,
    takeMVar,
  )
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (replicateM, (>=>))
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.Foldable (for_, traverse_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory (doesFileExist)
import System.FilePath (equalFilePath, normalise, splitDirectories)

-- | 'show' a value and take the result as 'Text'.
tshow :: (Show a) => a -> Text
tshow = T.pack . show

-- | Run an action, falling back on the given value if it throws.
quietly :: a -> IO a -> IO a
quietly fallback action =
  try action >>= \case
    Left (_ :: SomeException) -> pure fallback
    Right a -> pure a

-- | Read a file as UTF-8, leniently, if it is there and can be read.
readUtf8File :: FilePath -> IO (Maybe Text)
readUtf8File path = quietly Nothing $ do
  there <- doesFileExist path
  if there
    then Just . T.decodeUtf8Lenient <$> BS.readFile path
    else pure Nothing

-- | Write a file as UTF-8.
writeUtf8File :: FilePath -> Text -> IO ()
writeUtf8File path = BS.writeFile path . T.encodeUtf8

-- | The words of a field's value, which whitespace and commas separate.
fieldWords :: Text -> [Text]
fieldWords = filter (not . T.null) . concatMap (T.split (== ',')) . T.words

-- | Group lines into blocks, each a line at the margin with the indented
-- lines under it, dropping indented lines before the first.
blocksOf :: [Text] -> [(Text, [Text])]
blocksOf = \case
  [] -> []
  l : ls
    | indented l -> blocksOf ls
    | otherwise ->
        let (under, rest) = span indented ls
         in (l, under) : blocksOf rest

-- | Does a line begin with white space?
indented :: Text -> Bool
indented = maybe False (isSpace . fst) . T.uncons

-- | A path's segments under a directory, if it is at or under it.
pathUnder :: FilePath -> FilePath -> Maybe [FilePath]
pathUnder directory path =
  go (splitDirectories directory) (splitDirectories (normalise path))
  where
    go [] fs = Just fs
    go (d : ds) (f : fs) | equalFilePath d f = go ds fs
    go _ _ = Nothing

-- | Could a word name a module: does it begin with a capital letter and
-- hold no colon?
looksLikeModule :: Text -> Bool
looksLikeModule m = case T.uncons m of
  Just (c, _) -> c `elem` ['A' .. 'Z'] && not (T.any (== ':') m)
  Nothing -> False

-- | Run an action and return its result or the exception it threw rendered
-- as 'Text'.
attempted :: IO a -> IO (Either Text a)
attempted action =
  try action >>= \case
    Left (e :: SomeException) -> pure (Left (T.pack (displayException e)))
    Right a -> pure (Right a)

-- | Decode source as UTF-8.
asUtf8 :: BS.ByteString -> Either Text Text
asUtf8 bytes = case T.decodeUtf8' bytes of
  Right text -> Right text
  Left _ -> Left "it is not valid UTF-8"

-- | The byte order mark a text begins with, which GHC skips, if it has one,
-- and the rest of it.
byteOrderMark :: Text -> (Text, Text)
byteOrderMark t = T.splitAt (if "\xFEFF" `T.isPrefixOf` t then 1 else 0) t

-- | The SHA-256 of a text's UTF-8 encoding, in hexadecimal.
sha256Of :: Text -> Text
sha256Of = hex . SHA256.hash . T.encodeUtf8

-- | The SHA-256 of a file, in hexadecimal.
sha256OfFile :: FilePath -> IO Text
sha256OfFile = fmap (hex . SHA256.hashlazy) . BL.readFile

-- | Bytes as lower-case hexadecimal.
hex :: BS.ByteString -> Text
hex = T.decodeUtf8Lenient . B16.encode

-- | The line width for the terminal output of this program.
lineWidth :: Int
lineWidth = 76

-- | Indentation for the terminal output.
indent :: Int -> Text
indent level = T.replicate (2 * level) " "

-- | Break text into lines that fit the room given, at spaces.
wrapTo :: Int -> Text -> [Text]
wrapTo room = concatMap (go . T.words) . T.lines
  where
    go [] = []
    go (w : ws) = let (line, rest) = fill w ws in line : go rest
    fill line (w : ws)
      | visibleLength line + 1 + visibleLength w <= room =
          fill (line <> " " <> w) ws
    fill line ws = (line, ws)

-- | How wide a piece of text is once printed.
visibleLength :: Text -> Int
visibleLength = go 0
  where
    go !n t = case T.uncons t of
      Nothing -> n
      Just ('\ESC', rest)
        | Just after <- T.stripPrefix "[" rest ->
            go n (T.drop 1 (T.dropWhile (/= 'm') after))
      Just (_, rest) -> go (n + 1) rest

-- | Join items the way a sentence lists them: @a and b@, @a, b, and c@.
spellList :: [Text] -> Text
spellList items = case reverse items of
  [] -> ""
  [one] -> one
  [second, first'] -> first' <> " and " <> second
  (final : rest) -> T.intercalate ", " (reverse rest) <> ", and " <> final

-- | Collect the values filed under equal keys, keys in the order they first
-- appear.
collected :: (Eq k) => [(k, v)] -> [(k, [v])]
collected = foldl put []
  where
    put seen (k, v) = case break ((== k) . fst) seen of
      (before, (_, vs) : after) -> before <> [(k, vs <> [v])] <> after
      _ -> seen <> [(k, [v])]

-- | Run an action over every element at once, as far as the machine allows,
-- passing on an exception it throws.
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
  for_ done $ \signal -> forkFinally worker (putMVar signal)
  traverse_ (takeMVar >=> either throwIO pure) done
  Map.elems <$> readIORef answers
