{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Finding the project a file belongs to.
module Tilia.Cabal.Project
  ( ProjectRoot (..),
    Marker (..),
    markerFile,
    findProjectRoot,
    stackConfigAbove,
    stackOutputIn,
    startingDirectory,
    cabalFilesIn,
    projectConfigFiles,
  )
where

import Control.Monad (filterM, void)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.List (isSuffixOf, sort)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
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

import Tilia.Process (readProgramOutputIn)
import Tilia.Utils (quietly, tshow)

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

-- | Walk upwards from a file or directory looking for a project, having
-- Stack write the @.cabal@ file of a package that has only a
-- @package.yaml@.
findProjectRoot :: FilePath -> IO (Maybe ProjectRoot)
findProjectRoot start = quietly Nothing $ do
  from <- startingDirectory =<< canonicalizePath start
  found <- traverse markersIn (from : ancestorsOf from)
  pure (listToMaybe (concatMap fst found <> concatMap snd found))
  where
    markersIn directory = quietly ([], []) $ do
      entries <- listDirectory directory
      packages <-
        cabalFilesAmong directory entries >>= \case
          [] | "package.yaml" `elem` entries -> writtenByStack directory
          written -> pure written
      pure
        ( [ProjectRoot directory ProjectFile | "cabal.project" `elem` entries],
          [ ProjectRoot directory (PackageFile (takeFileName p))
          | p <- take 1 packages
          ]
        )

-- | The @.cabal@ files Stack writes from the @package.yaml@ in a directory,
-- where a @stack.yaml@ at or above it makes the package one Stack builds.
writtenByStack :: FilePath -> IO [FilePath]
writtenByStack directory =
  stackConfigAbove directory >>= \case
    Nothing -> pure []
    Just _ -> do
      void (stackOutputIn directory ["ide", "packages"])
      cabalFilesIn directory

-- | What Stack prints when run in a directory, never installing a compiler
-- or writing a lock file.
stackOutputIn :: FilePath -> [String] -> IO (Maybe Text)
stackOutputIn directory args =
  readProgramOutputIn directory "stack" $
    ["--no-install-ghc", "--lock-file", "read-only"] <> args

-- | The @stack.yaml@ in a directory or the nearest one above it.
stackConfigAbove :: FilePath -> IO (Maybe FilePath)
stackConfigAbove directory = quietly Nothing $ do
  from <- canonicalizePath directory
  listToMaybe
    <$> filterM
      doesFileExist
      [d </> "stack.yaml" | d <- from : ancestorsOf from]

-- | The directories above one, nearest first.
ancestorsOf :: FilePath -> [FilePath]
ancestorsOf directory =
  let parent = takeDirectory directory
   in if parent == directory then [] else parent : ancestorsOf parent

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
-- through one another, each once, with its fields or why they do not
-- parse.
--
-- A file that cannot be read, such as an import from a URL, is passed over.
projectConfigFiles :: FilePath -> IO [(FilePath, Either Text [Field Position])]
projectConfigFiles = go Set.empty . pure
  where
    go _ [] = pure []
    go seen (given : rest) = do
      file <- quietly given (canonicalizePath given)
      contents <- quietly Nothing (Just <$> BS.readFile file)
      case contents of
        Just bytes | Set.notMember file seen -> do
          let fields = first tshow (readFields bytes)
          ((file, fields) :)
            <$> go
              (Set.insert file seen)
              (importsOf file (fromRight [] fields) <> rest)
        _ -> go seen rest
    importsOf file fields =
      [ takeDirectory file </> T.unpack (T.decodeUtf8Lenient value)
      | Field (Name _ "import") [FieldLine _ value] <-
          concatMap fieldUniverse fields
      ]
