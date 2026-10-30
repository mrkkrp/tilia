{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Querying the compiler on which package holds a module and where its
-- compiled interfaces are.
module Tilia.Fixity.PackageDb
  ( InstalledPackage (..),
    Installed (..),
    readInstalledPackages,
    compilerIdentity,
    fromFields,
  )
where

import Control.Monad (filterM)
import Data.List (nub, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Compat.Newtype (unpack')
import Distribution.FieldGrammar.Newtypes (FSep (..), FilePathNT (..), alaList')
import Distribution.Parsec (Parsec, simpleParsec)
import System.Directory (doesDirectoryExist, findExecutable)
import System.FilePath ((</>))
import Tilia.Process (readProgramOutput)
import Tilia.Utils (blocksOf, fieldWords, looksLikeModule, quietly)

-- | An installed package.
data InstalledPackage = InstalledPackage
  { -- | Package name
    ipName :: Text,
    -- | Package version
    ipVersion :: Text,
    -- | Every module it holds, hidden ones included.
    ipModules :: [Text],
    -- | The modules it exposes that another package holds, each with its
    -- name there.
    ipReexports :: [(Text, Text)],
    -- | Where its compiled interfaces are.
    ipImportDirs :: [FilePath]
  }
  deriving (Eq, Show)

-- | What the compiler can see, and where it is.
data Installed = Installed
  { -- | Every package it can see.
    installedPackages :: [InstalledPackage],
    -- | The databases those were read from. Stamping these is how a later
    -- run tells whether what the compiler can see has changed.
    installedDatabases :: [FilePath]
  }
  deriving (Eq, Show)

-- | Everything the compiler can see, and where it read it from.
--
-- Empty if @ghc-pkg@ cannot be run, which leaves the packages that come with
-- the compiler unread.
--
-- @ghc-pkg@ is invoked rather than a database read off disk because where
-- the databases are is not knowable from outside: under Nix the wrapper
-- carries the paths, and @GHC_PACKAGE_PATH@ is not set. The records name
-- them, though, so having asked once we need not ask again to find out
-- whether the answer still holds.
readInstalledPackages :: IO Installed
readInstalledPackages =
  quietly (Installed [] []) $
    readProgramOutput "ghc-pkg" ["dump", "--global", "--user"] >>= \case
      Nothing -> pure (Installed [] [])
      Just out -> do
        let fields = fmap parseFields (records out)
        databases <- filterM doesDirectoryExist (databasesIn fields)
        pure
          Installed
            { installedPackages = mapMaybe fromFields fields,
              installedDatabases = databases
            }

-- | What distinguishes one compiler environment from another.
compilerIdentity :: IO Text
compilerIdentity =
  quietly "" (maybe "" T.pack <$> findExecutable "ghc-pkg")

-- | The databases a set of records came out of.
databasesIn :: [Map.Map Text Text] -> [FilePath]
databasesIn fields =
  nub [root </> "package.conf.d" | Just root <- fmap packageRoot fields]

-- | Where a record's database keeps what @${pkgroot}@ stands for.
packageRoot :: Map.Map Text Text -> Maybe FilePath
packageRoot = fmap getFilePathNT . parsedField "pkgroot"

-- | A record's field, read as Cabal-syntax reads it, which is how
-- @ghc-pkg@ writes it.
parsedField :: (Parsec a) => Text -> Map.Map Text Text -> Maybe a
parsedField key fields =
  simpleParsec . T.unpack . T.strip =<< Map.lookup key fields

-- | Split @ghc-pkg dump@ output into its records.
records :: Text -> [Text]
records = fmap T.unlines . go . T.lines
  where
    go ls = case break (== "---") ls of
      (record, []) -> [record | not (null record)]
      (record, _ : rest) -> record : go rest

-- | Read one record's fields, if they name a package.
fromFields :: Map.Map Text Text -> Maybe InstalledPackage
fromFields fields = do
  name <- Map.lookup "name" fields
  version <- Map.lookup "version" fields
  pure
    InstalledPackage
      { ipName = T.strip name,
        ipVersion = T.strip version,
        ipModules =
          concatMap
            (maybe [] moduleNames . (`Map.lookup` fields))
            ["exposed-modules", "hidden-modules"],
        ipReexports =
          maybe [] reexportedModules (Map.lookup "exposed-modules" fields),
        ipImportDirs =
          maybe
            []
            (fmap rooted . unpack' (alaList' FSep FilePathNT))
            (parsedField "import-dirs" fields)
      }
  where
    rooted path =
      fromMaybe path ((<>) <$> root <*> stripPrefix "${pkgroot}" path)
    root = packageRoot fields

-- | The modules an @exposed-modules@ field names that the package holds.
moduleNames :: Text -> [Text]
moduleNames = go . fieldWords
  where
    go = \case
      (_ : "from" : _ : rest) -> go rest
      (m : rest) | looksLikeModule m -> m : go rest
      (_ : rest) -> go rest
      [] -> []

-- | The modules an @exposed-modules@ field names that another package
-- holds, each with its name there.
reexportedModules :: Text -> [(Text, Text)]
reexportedModules = go . fieldWords
  where
    go = \case
      (m : "from" : original : rest) ->
        (m, T.takeWhileEnd (/= ':') original) : go rest
      (_ : rest) -> go rest
      [] -> []

-- | Split a record into its fields.
parseFields :: Text -> Map.Map Text Text
parseFields = Map.fromList . mapMaybe field . blocksOf . T.lines
  where
    field (l, rest) = case T.breakOn ":" l of
      (key, value)
        | not (T.null value) ->
            Just (T.strip key, T.unwords (T.drop 1 value : fmap T.strip rest))
      _ -> Nothing
