{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading a package's description out of its source tarball.
module Tilia.Fixity.Cabal
  ( packageModules,
    cabalFileInArchive,
    ownCabalFile,
    cabalFileAtTop,
    entryPosixPath,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as BL
import Data.List (isSuffixOf)
import Data.Text (Text)
import Distribution.PackageDescription (GenericPackageDescription)
import Tilia.Cabal.Package
  ( declaredModules,
    packageBranches,
    parsePackageDescription,
  )
import Tilia.Utils (quietly)

-- | The modules a package holds, read from the @.cabal@ file in its
-- source tarball.
--
-- 'Nothing' if the tarball cannot be read or holds no @.cabal@ file.
packageModules :: FilePath -> IO (Maybe [Text])
packageModules tarball = quietly Nothing $ do
  bytes <- BL.readFile tarball
  evaluate . force $
    fmap fst . concatMap snd . declaredModules . packageBranches
      <$> cabalFileInArchive (Tar.read (GZip.decompress bytes))

-- | The first @.cabal@ file at the top level of an archive.
--
-- Stops as soon as it finds one. The archive is decompressed lazily, so a
-- @.cabal@ near the front costs a fraction of the whole file.
cabalFileInArchive :: Tar.Entries e -> Maybe GenericPackageDescription
cabalFileInArchive = \case
  Tar.Next entry rest -> ownCabalFile entry <|> cabalFileInArchive rest
  Tar.Done -> Nothing
  Tar.Fail _ -> Nothing

-- | What an archive's entry describes if it is the package's own @.cabal@
-- file and parses.
ownCabalFile :: Tar.Entry -> Maybe GenericPackageDescription
ownCabalFile entry
  | cabalFileAtTop path,
    Tar.NormalFile content _ <- Tar.entryContent entry =
      either (const Nothing) Just $
        parsePackageDescription path (BL.toStrict content)
  | otherwise = Nothing
  where
    path = entryPosixPath entry

-- | Where an entry sits in its archive, written the way a tar file writes
-- it.
entryPosixPath :: Tar.Entry -> FilePath
entryPosixPath = Tar.fromTarPathToPosixPath . Tar.entryTarPath

-- | Is this the path of a package's own @.cabal@ file?
cabalFileAtTop :: FilePath -> Bool
cabalFileAtTop path = ".cabal" `isSuffixOf` path && depth path == 2
  where
    depth = (1 +) . length . filter (== '/')
