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
    projectConfigFiles,
  )
where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.List (isSuffixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Distribution.Fields.Field
  ( Field (..),
    FieldLine (..),
    Name (..),
    fieldUniverse,
  )
import Distribution.Fields.Parser (readFields)
import Distribution.Parsec.Position (Position)
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

-- | A project configuration file and the files it imports, directly or
-- through one another, each once, with its fields.
--
-- A file that cannot be read, such as an import from a URL, is passed over.
projectConfigFiles :: FilePath -> IO [(FilePath, [Field Position])]
projectConfigFiles = go Set.empty . pure
  where
    go _ [] = pure []
    go seen (given : rest) = do
      file <- quietly given (canonicalizePath given)
      contents <- quietly Nothing (Just <$> BS.readFile file)
      case contents of
        Just bytes | Set.notMember file seen -> do
          let fields = fromRight [] (readFields bytes)
          ((file, fields) :)
            <$> go (Set.insert file seen) (importsOf file fields <> rest)
        _ -> go seen rest
    importsOf file fields =
      [ takeDirectory file </> T.unpack (T.decodeUtf8Lenient value)
      | Field (Name _ "import") [FieldLine _ value] <-
          concatMap fieldUniverse fields
      ]
