{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Finding the project a file belongs to.
module Tilia.Cabal.Project
  ( ProjectRoot (..),
    Marker (..),
    markerFile,
    findProjectRoot,
  )
where

import Data.List (isSuffixOf)
import Data.Maybe (listToMaybe)
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    listDirectory,
  )
import System.FilePath (takeDirectory)
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
  from <- startingDirectory
  found <- traverse markersIn (from : ancestorsOf from)
  pure (listToMaybe (concatMap fst found <> concatMap snd found))
  where
    startingDirectory = do
      absolute <- canonicalizePath start
      isDirectory <- doesDirectoryExist absolute
      pure (if isDirectory then absolute else takeDirectory absolute)

    ancestorsOf directory =
      let parent = takeDirectory directory
       in if parent == directory then [] else parent : ancestorsOf parent

    markersIn directory = quietly ([], []) $ do
      entries <- listDirectory directory
      pure
        ( [ProjectRoot directory ProjectFile | "cabal.project" `elem` entries],
          [ ProjectRoot directory (PackageFile named)
          | named <- take 1 (filter (".cabal" `isSuffixOf`) entries)
          ]
        )
