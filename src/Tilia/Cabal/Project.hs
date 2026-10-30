{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Finding the project a file belongs to.
module Tilia.Cabal.Project
  ( ProjectRoot (..),
    Marker (..),
    markerFile,
    findProjectRoot,
    startingDirectory,
    cabalFilesIn,
  )
where

import Control.Monad (filterM)
import Data.List (isSuffixOf, sort)
import Data.Maybe (listToMaybe)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (takeDirectory, takeFileName, (</>))
import Tilia.Utils (quietly)

-- | A project, and what marked it out.
data ProjectRoot = ProjectRoot
  { -- | The directory.
    prPath :: FilePath,
    -- | What identified it.
    prMarker :: Marker
  }
  deriving (Eq, Show)

-- | What a project was recognised by.
data Marker
  = -- | A @cabal.project@, which names the packages.
    ProjectFile
  | -- | A @.cabal@ file, by its name: a package with no project around it.
    PackageFile FilePath
  deriving (Eq, Show)

-- | The file a marker stands for, for reporting.
markerFile :: Marker -> FilePath
markerFile = \case
  ProjectFile -> "cabal.project"
  PackageFile named -> named

-- | Walk upwards from a file or directory looking for a project.
findProjectRoot :: FilePath -> IO (Maybe ProjectRoot)
findProjectRoot start = quietly Nothing $ do
  from <- startingDirectory =<< canonicalizePath start
  found <- traverse markersIn (from : ancestorsOf from)
  pure (listToMaybe (concatMap fst found <> concatMap snd found))
  where
    ancestorsOf directory =
      let parent = takeDirectory directory
       in if parent == directory then [] else parent : ancestorsOf parent

    markersIn directory = quietly ([], []) $ do
      entries <- listDirectory directory
      packages <- cabalFilesAmong directory entries
      pure
        ( [ProjectRoot directory ProjectFile | "cabal.project" `elem` entries],
          [ProjectRoot directory (PackageFile (takeFileName p)) | p <- take 1 packages]
        )

-- | Where a walk upwards from a path starts: the path itself if it names a
-- directory, else the directory holding it.
startingDirectory :: FilePath -> IO FilePath
startingDirectory path = do
  isDirectory <- quietly False (doesDirectoryExist path)
  pure (if isDirectory then path else takeDirectory path)

-- | The @.cabal@ files sitting directly in a directory, in order.
cabalFilesIn :: FilePath -> IO [FilePath]
cabalFilesIn directory =
  quietly [] (cabalFilesAmong directory =<< listDirectory directory)

-- | The @.cabal@ files among a directory's entries, in order.
cabalFilesAmong :: FilePath -> [FilePath] -> IO [FilePath]
cabalFilesAmong directory entries =
  filterM
    doesFileExist
    (fmap (directory </>) (sort (filter (".cabal" `isSuffixOf`) entries)))
