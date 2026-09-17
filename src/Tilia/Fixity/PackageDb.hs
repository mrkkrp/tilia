{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Querying the compiler on which package exposes a module and where its
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
import Data.Char (isSpace)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, findExecutable)
import System.FilePath ((</>))
import Tilia.Process (readProgramOutput)
import Tilia.Utils (quietly)

-- | An installed package.
data InstalledPackage = InstalledPackage
  { -- | Package name
    ipName :: Text,
    -- | Package version
    ipVersion :: Text,
    -- | Every module it holds, hidden ones included, with re-export clauses
    -- dropped.
    ipModules :: [Text],
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
-- Empty if @ghc-pkg@ cannot be run, which is not fatal.
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
        let fields = map parseFields (records out)
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
  nub
    [ T.unpack (unquote root) </> "package.conf.d"
    | f <- fields,
      Just root <- [Map.lookup "pkgroot" f]
    ]

-- | Strip the quotes a path is written in when it has none needing them.
unquote :: Text -> Text
unquote = T.dropAround (== '"') . T.strip

-- | Split @ghc-pkg dump@ output into its records.
records :: Text -> [Text]
records = map T.unlines . go . T.lines
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
        ipImportDirs =
          maybe
            []
            (map (T.unpack . rooted fields . unquote) . T.words)
            (Map.lookup "import-dirs" fields)
      }

-- | Put the package's root where its registration only left a variable.
rooted :: Map.Map Text Text -> Text -> Text
rooted fields path = case Map.lookup "pkgroot" fields of
  Nothing -> path
  Just root -> T.replace "${pkgroot}" (unquote root) path

-- | The module names in an @exposed-modules@ field.
moduleNames :: Text -> [Text]
moduleNames = go . filter (not . T.null) . concatMap (T.split (== ',')) . T.words
  where
    go = \case
      (_ : "from" : _ : rest) -> go rest
      (m : rest) | looksLikeModule m -> m : go rest
      (_ : rest) -> go rest
      [] -> []
    looksLikeModule m = case T.uncons m of
      Just (c, _) -> c `elem` ['A' .. 'Z'] && not (T.any (== ':') m)
      Nothing -> False

-- | Split a record into its fields.
parseFields :: Text -> Map.Map Text Text
parseFields = Map.fromList . mapMaybe field . groups . T.lines
  where
    groups = \case
      [] -> []
      (l : ls)
        | isContinuation l -> groups ls
        | otherwise ->
            let (continued, rest) = span isContinuation ls
             in (l : continued) : groups rest
    isContinuation l = not (T.null l) && isSpace (T.head l)

    field [] = Nothing
    field (l : rest) = case T.breakOn ":" l of
      (key, value)
        | not (T.null value) ->
            Just (T.strip key, T.unwords (T.drop 1 value : map T.strip rest))
      _ -> Nothing
