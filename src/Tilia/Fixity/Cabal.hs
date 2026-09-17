{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading a package's exposed modules out of its @.cabal@ file.
module Tilia.Fixity.Cabal
  ( packageModules,
    cabalFileInArchive,
    cabalFileAtTop,
    entryPosixPath,
    containedModules,
    sourceDirs,
    declaredExtensions,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.List (isSuffixOf)
import Data.List qualified
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension)
import Tilia.Pragma (lookupExtension)
import Tilia.Utils (quietly)

-- | The modules a package exposes, read from the @.cabal@ file in its
-- source tarball.
--
-- 'Nothing' if the tarball cannot be read or holds no @.cabal@ file.
packageModules :: FilePath -> IO (Maybe [Text])
packageModules tarball = quietly Nothing $ do
  bytes <- BL.readFile tarball
  pure (containedModules <$> cabalFileInArchive (Tar.read (GZip.decompress bytes)))

-- | The first @.cabal@ file at the top level of an archive.
--
-- Stops as soon as it finds one. The archive is decompressed lazily, so a
-- @.cabal@ near the front costs a fraction of the whole file.
cabalFileInArchive :: Tar.Entries e -> Maybe Text
cabalFileInArchive = \case
  Tar.Next entry rest
    | cabalFileAtTop (entryPosixPath entry),
      Tar.NormalFile content _ <- Tar.entryContent entry ->
        Just (T.decodeUtf8Lenient (BL.toStrict content))
    | otherwise -> cabalFileInArchive rest
  Tar.Done -> Nothing
  Tar.Fail _ -> Nothing

-- | Where an entry sits in its archive, written the way a tar file writes
-- it.
entryPosixPath :: Tar.Entry -> FilePath
entryPosixPath = Tar.fromTarPathToPosixPath . Tar.entryTarPath

-- | Is this the path of a package's own @.cabal@ file?
cabalFileAtTop :: FilePath -> Bool
cabalFileAtTop path = ".cabal" `isSuffixOf` path && depth path == 2
  where
    depth = (1 +) . length . filter (== '/')

-- | Every module a package holds, whether it exposes it or not.
containedModules :: Text -> [Text]
containedModules t =
  modulesUnder "exposed-modules" t <> modulesUnder "other-modules" t
  where
    modulesUnder field =
      concatMap moduleNames . fieldsNamed field . T.lines
      where
        moduleNames =
          filter looksLikeModule
            . concatMap (T.split (== ','))
            . T.words
        looksLikeModule m = case T.uncons m of
          Just (c, _) -> c `elem` ['A' .. 'Z']
          Nothing -> False

-- | Every directory a @.cabal@ file's modules could be under.
sourceDirs :: Text -> [Text]
sourceDirs contents = Data.List.nub (named <> ["."])
  where
    named =
      filter (not . T.null)
        . fmap T.strip
        . concatMap (T.split (== ','))
        . concatMap T.words
        . fieldsNamed "hs-source-dirs"
        $ T.lines contents

-- | The extensions a package puts in force, read from its @.cabal@ file.
declaredExtensions :: Text -> [Extension]
declaredExtensions contents =
  foldl apply baseline named
  where
    ls = T.lines contents
    baseline = case mapMaybe languageNamed (fieldsNamed "default-language" ls) of
      [] -> GHC.languageExtensions Nothing
      editions ->
        Data.List.nub (concatMap (GHC.languageExtensions . Just) editions)
    named =
      concatMap (T.split (== ',')) . concatMap T.words $
        fieldsNamed "default-extensions" ls
    apply acc written = case T.strip written of
      name
        | Just off <- T.stripPrefix "No" name,
          Just extension <- lookupExtension off ->
            filter (/= extension) acc
        | Just extension <- lookupExtension name,
          extension `notElem` acc ->
            acc <> [extension]
        | otherwise -> acc
    languageNamed written =
      lookup (T.unpack (T.strip written)) [(show e, e) | e <- [minBound .. maxBound]]

-- | The values of every field with the given name, wherever it appears and
-- however deeply it is nested.
fieldsNamed :: Text -> [Text] -> [Text]
fieldsNamed name = go . filter (not . commented)
  where
    commented = T.isPrefixOf "--" . T.stripStart
    go = \case
      [] -> []
      (l : ls)
        | Just value <- fieldValue l ->
            let (continued, rest) = span (deeperThan (indentOf l)) ls
             in T.unwords (value : fmap T.strip continued) : go rest
        | otherwise -> go ls
    fieldValue l =
      let (key, rest) = T.break (== ':') l
       in if T.toLower (T.strip key) == name && not (T.null rest)
            then Just (T.strip (T.drop 1 rest))
            else Nothing
    deeperThan n l = T.null (T.strip l) || indentOf l > n
    indentOf = T.length . T.takeWhile isSpace
