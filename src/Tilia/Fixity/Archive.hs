{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a package's source tarball holds that a module could be read out
-- of.
module Tilia.Fixity.Archive
  ( Archive (..),
    readArchive,
    InArchive (..),
    moduleIn,
    moduleEndings,
    writtenForHsc,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Generics (Generic)
import Tilia.Fixity.Cabal (cabalFileAtTop, entryPosixPath, sourceDirs)
import Tilia.Utils (quietly)

-- | What a tarball holds that a module could be read out of.
data Archive = Archive
  { -- | The package's @.cabal@ file, the first one at the top.
    archiveCabal :: Maybe Text,
    -- | Every file that could hold a module, by where it sits, in the order
    -- the tarball has them.
    archiveFiles :: [(FilePath, BS.ByteString)]
  }
  deriving (Generic)

instance NFData Archive

-- | Read a tarball, keeping what a module could be read out of.
readArchive :: FilePath -> IO (Maybe Archive)
readArchive tarball = quietly Nothing $ do
  bytes <- BL.readFile tarball
  Just <$> evaluate (force (sweep Nothing [] (Tar.read (GZip.decompress bytes))))
  where
    sweep cabal found = \case
      Tar.Next entry rest
        | Tar.NormalFile content _ <- Tar.entryContent entry,
          cabalFileAtTop (entryPosixPath entry),
          Nothing <- cabal ->
            sweep (Just (T.decodeUtf8Lenient (BL.toStrict content))) found rest
        | Tar.NormalFile content _ <- Tar.entryContent entry,
          any (`isSuffixOf` entryPosixPath entry) moduleEndings ->
            sweep cabal ((entryPosixPath entry, BL.toStrict content) : found) rest
        | otherwise -> sweep cabal found rest
      _ -> Archive cabal (reverse found)

-- | Find a module in what a tarball holds and say what was found.
moduleIn :: Text -> Archive -> Maybe InArchive
moduleIn modName archive = listToMaybe (mapMaybe pick moduleEndings)
  where
    dirs = maybe [] sourceDirs (archiveCabal archive)
    pick ending =
      inArchive ending . T.decodeUtf8Lenient . snd
        <$> listToMaybe (under sfx matching <> matching)
      where
        sfx = "/" <> T.unpack (T.replace "." "/" modName) <> ending
        matching = [f | f <- archiveFiles archive, sfx `isSuffixOf` fst f]
    under sfx matching = [f | d <- dirs, f <- matching, inDir sfx d (fst f)]
    inDir sfx d path
      | d == "." = takeWhile (/= '/') path <> sfx == path
      | otherwise = ("/" <> T.unpack d <> sfx) `isSuffixOf` path

-- | The endings a package may write a module under, in the order they are
-- tried.
moduleEndings :: [String]
moduleEndings = [".hs", ".hsc"]

-- | What an archive holds for a module.
data InArchive
  = -- | Haskell, as the package wrote it.
    Haskell Text
  | -- | A module written for @hsc2hs@. Its text is not kept: there is
    -- nothing to be done with it, and 'hscFixities' answers for it.
    ForHsc

-- | What was found under one ending amounts to.
inArchive :: String -> Text -> InArchive
inArchive ending text
  | writtenForHsc ending = ForHsc
  | otherwise = Haskell text

-- | Is this a module @hsc2hs@ writes rather than one anybody compiles?
writtenForHsc :: FilePath -> Bool
writtenForHsc = isSuffixOf ".hsc"
