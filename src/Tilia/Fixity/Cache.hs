{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Package-level cache.
module Tilia.Fixity.Cache
  ( Cache,
    PlanToken (..),
    openCache,
    recalled,
    cachedModules,
    storeModules,
    cachedFixities,
    storeFixities,
    cachedExportNames,
    storeExportNames,
    cachedChildren,
    storeChildren,
    cachedSummaries,
    storeSummaries,
    cachedInstalled,
    storeInstalled,
    cachedFutileSolve,
    storeFutileSolve,
    cachedFutileFetch,
    storeFutileFetch,
  )
where

import Control.Monad (join)
import Data.Choice (Choice, isFalse)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Read qualified as T
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    getModificationTime,
    getXdgDirectory,
    renameFile,
  )
import System.FilePath (takeDirectory, (</>))
import Tilia.Fixity
import Tilia.Fixity.PackageDb (Installed (..), InstalledPackage (..))
import Tilia.Utils (quietly)

-- | Where cached answers are kept together with a token unique to this
-- build plan and environment, or nowhere, in which case nothing is
-- remembered.
data Cache = Cache FilePath PlanToken | NoCache

-- | A token that is unique to this plan and environment. It is needed in
-- order to be able to cache the expensive class of lookup failures that are
-- related to chasing module re-export chains. Rather than track what each
-- failure leaned on, all of them are tied to the plan and the environment
-- as a whole.
--
-- The environment belongs in it because availability of a module the plan
-- names depends on where we run the query and that is settled outside the
-- project. See 'Tilia.Fixity.PackageDb.compilerIdentity'.
newtype PlanToken = PlanToken Text
  deriving (Eq, Show)

-- | Bumped whenever cache format changes.
formatVersion :: FilePath
formatVersion = "v2"

-- | Open, creating the directory if need be.
--
-- 'NoCache' if the cache is not to be used or there is nowhere to write, in
-- which case everything still works and is merely slower.
openCache :: Choice "useCache" -> PlanToken -> IO Cache
openCache use token
  | isFalse use = pure NoCache
  | otherwise = quietly NoCache $ do
      root <- (</> formatVersion) <$> getXdgDirectory XdgCache "tilia"
      createDirectoryIfMissing True root
      pure (Cache root token)

-- | What was remembered, or else what reading finds, remembered in turn.
recalled ::
  -- | Recall the answer.
  IO (Maybe a) ->
  -- | Remember one.
  (a -> IO ()) ->
  -- | Work the answer out, if there is one to be had.
  IO (Maybe a) ->
  IO (Maybe a)
recalled recall remember work =
  recall >>= \case
    Just answer -> pure (Just answer)
    Nothing -> do
      found <- work
      traverse_ remember found
      pure found

-- | The modules a package exposes, if that was worked out before.
cachedModules :: Cache -> Text -> IO (Maybe [Text])
cachedModules cache package =
  readIfPresent (at cache ["modules", package]) $
    filter (not . T.null) . T.lines

-- | Remember what a package exposes.
storeModules :: Cache -> Text -> [Text] -> IO ()
storeModules cache package =
  writeAtomically (at cache ["modules", package]) . T.unlines

-- | What was established about a module before, if anything.
cachedFixities ::
  -- | Where to look.
  Cache ->
  -- | The package the module belongs to. Opaque here.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  -- | What was established, or 'Nothing' if nothing was.
  IO (Maybe Established)
cachedFixities cache package modName =
  fmap join . readIfPresent (at cache ["fixities", package, modName]) $ \contents ->
    case T.lines contents of
      ("read" : entries) -> Declares . Map.fromList <$> traverse parseFixity entries
      [unread] | Just rest <- T.stripPrefix ("unread\t" <> tokenOf cache) unread ->
        case T.uncons rest of
          Nothing -> Just (Unreadable Nothing)
          Just ('\t', below) | not (T.null below) -> Just (Unreadable (Just below))
          _ -> Nothing
      _ -> Nothing

-- | Remember what reading a module established.
storeFixities ::
  -- | Where to write.
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  -- | What was established about it.
  Established ->
  IO ()
storeFixities cache package modName answer =
  writeAtomically (at cache ["fixities", package, modName]) $
    case answer of
      Unreadable below ->
        T.unlines ["unread\t" <> tokenOf cache <> foldMap ("\t" <>) below]
      Declares fixities ->
        T.unlines ("read" : fmap renderFixity (Map.toList fixities))

-- | What a module's export list was found to say, if it was ever read:
-- the operators it names, or 'Nothing' where they cannot be enumerated.
cachedExportNames ::
  -- | Where to look.
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  IO (Maybe (Maybe (Set OpName)))
cachedExportNames cache package modName =
  fmap join . readIfPresent (at cache ["exports", package, modName]) $ \contents ->
    case T.lines contents of
      ("names" : entries) -> Just (Just (Set.fromList (fmap OpName entries)))
      ["untellable"] -> Just Nothing
      _ -> Nothing

-- | Remember what a module's export list said.
storeExportNames ::
  -- | Where to write.
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  -- | The operators it names, or 'Nothing' where they cannot be enumerated.
  Maybe (Set OpName) ->
  IO ()
storeExportNames cache package modName answer =
  writeAtomically (at cache ["exports", package, modName]) $
    case answer of
      Nothing -> T.unlines ["untellable"]
      Just names -> T.unlines ("names" : [op | OpName op <- Set.toAscList names])

-- | What a module keeps under each of its names, if it was ever read for
-- it.
cachedChildren ::
  -- | Where to look.
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  -- | What it keeps under each name, or 'Nothing' if it was never read.
  IO (Maybe (Map OpName (Set OpName)))
cachedChildren cache package modName =
  fmap join . readIfPresent (at cache ["children", package, modName]) $ \contents ->
    case T.lines contents of
      ("children" : entries) -> Just (Map.fromList (mapMaybe childEntry entries))
      _ -> Nothing
  where
    childEntry line = case T.splitOn "\t" line of
      (parent : kids) -> Just (OpName parent, Set.fromList (fmap OpName kids))
      [] -> Nothing

-- | Remember what a module keeps under each of its names.
storeChildren ::
  -- | Where to write.
  Cache ->
  -- | The package the module belongs to, as 'cachedFixities' takes it.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  -- | What it keeps under each name.
  Map OpName (Set OpName) ->
  IO ()
storeChildren cache package modName children =
  writeAtomically (at cache ["children", package, modName]) $
    T.unlines ("children" : fmap entry (Map.toList children))
  where
    entry (OpName parent, kids) =
      T.intercalate "\t" (parent : [kid | OpName kid <- Set.toAscList kids])

-- | What each configuration of one of the project's own modules says, if it
-- was last read from what the stamp stands for.
cachedSummaries ::
  -- | Where to look.
  Cache ->
  -- | Which module, by a name for its file.
  Text ->
  -- | What it was read from: its text and the settings it was read with.
  Text ->
  -- | What it said, or 'Nothing' if nothing is remembered for that stamp.
  IO (Maybe (Maybe (NonEmpty ModuleSummary)))
cachedSummaries cache key stamp =
  fmap join . readIfPresent (at cache ["summaries", key]) $ \contents ->
    case T.lines contents of
      (header : rest) | header == readFrom stamp -> parseSummaries rest
      _ -> Nothing

-- | Remember what each configuration of one of the project's own modules
-- says, in place of what an earlier text of it said.
storeSummaries ::
  -- | Where to write.
  Cache ->
  -- | Which module, as 'cachedSummaries' takes it.
  Text ->
  -- | What it was read from, as 'cachedSummaries' takes it.
  Text ->
  -- | What it said, or 'Nothing' where none of its configurations parses.
  Maybe (NonEmpty ModuleSummary) ->
  IO ()
storeSummaries cache key stamp summaries =
  writeAtomically (at cache ["summaries", key]) . T.unlines $
    readFrom stamp : renderSummaries summaries

-- | The first line of a remembered summary: what it was read from, and the
-- versions of Tilia and of the parser that read it.
readFrom :: Text -> Text
readFrom stamp =
  T.intercalate "\t" ["for", stamp, VERSION_tilia, VERSION_ghc_lib_parser]

-- | Render what a module's configurations say, a line for each thing.
renderSummaries :: Maybe (NonEmpty ModuleSummary) -> [Text]
renderSummaries = \case
  Nothing -> ["unparsed"]
  Just summaries -> concatMap (("configuration" :) . renderSummary) (toList summaries)
  where
    renderSummary s =
      ["name\t" <> name | Just name <- [summaryName s]]
        <> maybe [] (\items -> "exports" : fmap renderExport items) (summaryExports s)
        <> concatMap renderImport (summaryImports s)
        <> fmap (("fixity\t" <>) . renderFixity) (Map.toList (summaryFixities s))
        <> [ T.intercalate "\t" ["defines", renderNamespace namespace, op]
           | (namespace, OpName op) <- Set.toAscList (summaryNames s)
           ]
        <> fmap declares (Map.toList (summaryDeclaredChildren s))
        <> fmap offers (Map.toList (summaryChildren s))
    declares (OpName parent, kids) =
      T.intercalate "\t" ("declares" : parent : renderNamespaced kids)
    offers (OpName parent, kids) =
      T.intercalate "\t" ("offers" : parent : [kid | OpName kid <- Set.toAscList kids])
    renderExport = \case
      ExportName namespace qualifier (OpName op) ->
        T.intercalate "\t" ["export", "name", renderNamespace namespace, fromMaybe "" qualifier, op]
      ExportAll qualifier (OpName op) ->
        T.intercalate "\t" ["export", "all", fromMaybe "" qualifier, op]
      ExportSome qualifier (OpName op) kids ->
        T.intercalate "\t" (["export", "some", fromMaybe "" qualifier, op] <> [kid | OpName kid <- kids])
      ExportModule m -> "export\tmodule\t" <> m
    renderImport i =
      T.intercalate
        "\t"
        ["import", importModule i, if importQualified i then "qualified" else "open", importAlias i]
        : case importNames i of
          Nothing -> []
          Just (hiding, items) ->
            (if hiding then "list\thiding" else "list\tonly") : fmap renderItem items
    renderItem = \case
      ImportedName (OpName op) -> "item\tname\t" <> op
      ImportedAll (OpName op) -> "item\tall\t" <> op
      ImportedSome (OpName op) kids ->
        T.intercalate "\t" ("item" : "some" : op : [kid | OpName kid <- kids])

-- | Parse what 'renderSummaries' rendered.
parseSummaries :: [Text] -> Maybe (Maybe (NonEmpty ModuleSummary))
parseSummaries = \case
  ["unparsed"] -> Just Nothing
  ls -> Just <$> (NE.nonEmpty =<< traverse parseSummary =<< configurations ls)
  where
    configurations = \case
      [] -> Just []
      "configuration" : rest ->
        let (these, more) = break (== "configuration") rest
         in (these :) <$> configurations more
      _ -> Nothing
    parseSummary = go empty . fmap (T.splitOn "\t")
      where
        empty = ModuleSummary Nothing Nothing [] Map.empty Set.empty Map.empty Map.empty
    go s = \case
      [] ->
        Just
          s
            { summaryExports = reverse <$> summaryExports s,
              summaryImports = reverse (summaryImports s)
            }
      ["name", name] : rest -> go s{summaryName = Just name} rest
      ["exports"] : rest -> go s{summaryExports = Just []} rest
      ("export" : fields) : rest -> do
        item <- exportItem fields
        items <- summaryExports s
        go s{summaryExports = Just (item : items)} rest
      ["import", m, how, alias] : rest -> do
        qualified <- case how of
          "qualified" -> Just True
          "open" -> Just False
          _ -> Nothing
        let (listed, rest') = span isListed rest
        list <- importList listed
        go s{summaryImports = Import m qualified alias list : summaryImports s} rest'
      ("fixity" : fields) : rest -> do
        (key, fixity) <- parseFixity (T.intercalate "\t" fields)
        go s{summaryFixities = Map.insert key fixity (summaryFixities s)} rest
      ["defines", namespace, op] : rest -> do
        n <- parseNamespace namespace
        go s{summaryNames = Set.insert (n, OpName op) (summaryNames s)} rest
      ("declares" : parent : kids) : rest -> do
        namespaced <- parseNamespaced kids
        go s{summaryDeclaredChildren = Map.insert (OpName parent) (Set.fromList namespaced) (summaryDeclaredChildren s)} rest
      ("offers" : parent : kids) : rest ->
        go s{summaryChildren = Map.insert (OpName parent) (names kids) (summaryChildren s)} rest
      _ -> Nothing
    names = Set.fromList . fmap OpName
    qualifier q = if T.null q then Nothing else Just q
    exportItem = \case
      ["name", namespace, q, op] ->
        (\n -> ExportName n (qualifier q) (OpName op)) <$> parseNamespace namespace
      ["all", q, op] -> Just (ExportAll (qualifier q) (OpName op))
      "some" : q : op : kids -> Just (ExportSome (qualifier q) (OpName op) (fmap OpName kids))
      ["module", m] -> Just (ExportModule m)
      _ -> Nothing
    isListed = \case
      "list" : _ -> True
      "item" : _ -> True
      _ -> False
    importList = \case
      [] -> Just Nothing
      ["list", way] : items -> do
        hiding <- case way of
          "hiding" -> Just True
          "only" -> Just False
          _ -> Nothing
        Just . (,) hiding <$> traverse importItem items
      _ -> Nothing
    importItem = \case
      ["item", "name", op] -> Just (ImportedName (OpName op))
      ["item", "all", op] -> Just (ImportedAll (OpName op))
      "item" : "some" : op : kids -> Just (ImportedSome (OpName op) (fmap OpName kids))
      _ -> Nothing

-- | What packages the compiler could see when last asked, if it can still
-- see it.
cachedInstalled :: Cache -> IO (Maybe [InstalledPackage])
cachedInstalled cache = quietly Nothing $ do
  readIfPresent (at cache ["installed", tokenOf cache]) T.lines >>= \case
    Nothing -> pure Nothing
    Just ls -> do
      let written =
            [(T.unpack path, stamp) | ["db", path, stamp] <- fmap fields ls]
      still <- traverse unchanged written
      pure $
        if not (null written) && and still
          then Just (mapMaybe installedFrom ls)
          else Nothing
  where
    unchanged (path, stamp) =
      quietly False ((== stamp) . T.pack . show <$> getModificationTime path)
    installedFrom l = case fields l of
      ("pkg" : name : version : modules : dirs) ->
        Just
          InstalledPackage
            { ipName = name,
              ipVersion = version,
              ipModules = T.words modules,
              ipImportDirs = fmap T.unpack dirs
            }
      _ -> Nothing
    fields = T.splitOn "\t"

-- | Remember a package the compiler can see, stamped so that a later run
-- can tell whether it still does.
storeInstalled :: Cache -> Installed -> IO ()
storeInstalled cache found
  | null (installedDatabases found) = pure ()
  | otherwise = quietly () $ do
      stamps <- traverse stamped (installedDatabases found)
      writeAtomically (at cache ["installed", tokenOf cache]) . T.unlines $
        [T.intercalate "\t" ["db", T.pack path, stamp] | (path, stamp) <- stamps]
          <> [ T.intercalate "\t" $
                 ["pkg", ipName p, ipVersion p, T.unwords (ipModules p)]
                   <> fmap T.pack (ipImportDirs p)
             | p <- installedPackages found
             ]
  where
    stamped path = do
      stamp <- T.pack . show <$> getModificationTime path
      pure (path, stamp)

-- | Whether asking @cabal@ to solve this plan again has already been tried
-- and left the plan exactly as before.
cachedFutileSolve :: Cache -> IO Bool
cachedFutileSolve cache =
  isJust <$> readIfPresent (at cache ["solves", tokenOf cache]) (const ())

-- | Remember that solving again did not widen the plan.
storeFutileSolve :: Cache -> IO ()
storeFutileSolve cache =
  writeAtomically (at cache ["solves", tokenOf cache]) ""

-- | The packages an earlier run was still short of after asking @cabal@ to
-- fetch them.
cachedFutileFetch :: Cache -> IO [Text]
cachedFutileFetch cache =
  fromMaybe []
    <$> readIfPresent
      (at cache ["fetches", tokenOf cache])
      (filter (not . T.null) . T.lines)

-- | Remember what fetching left missing.
storeFutileFetch :: Cache -> [Text] -> IO ()
storeFutileFetch cache =
  writeAtomically (at cache ["fetches", tokenOf cache]) . T.unlines

-- | Render a fixity declaration as 'Text'.
renderFixity :: ((Namespace, OpName), Fixity) -> Text
renderFixity ((namespace, OpName op), Fixity direction precedence) =
  T.intercalate
    "\t"
    [op, renderNamespace namespace, renderDirection direction, T.pack (show precedence)]
  where
    renderDirection = \case
      LeftAssoc -> "l"
      RightAssoc -> "r"
      NoAssoc -> "n"

-- | Parse a fixity declaration from 'Text'.
parseFixity :: Text -> Maybe ((Namespace, OpName), Fixity)
parseFixity line = case T.splitOn "\t" line of
  [op, namespace, direction, precedence] -> do
    n <- parseNamespace namespace
    d <- parseDirection direction
    p <- readPrecedence precedence
    pure ((n, OpName op), Fixity d p)
  _ -> Nothing
  where
    parseDirection = \case
      "l" -> Just LeftAssoc
      "r" -> Just RightAssoc
      "n" -> Just NoAssoc
      _ -> Nothing
    readPrecedence t = case T.signed T.decimal t of
      Right (p, rest) | T.null rest -> Just p
      _ -> Nothing

-- | Render a namespace as 'Text'.
renderNamespace :: Namespace -> Text
renderNamespace = \case
  InTypes -> "t"
  InTerms -> "v"

-- | Parse a namespace from 'Text'.
parseNamespace :: Text -> Maybe Namespace
parseNamespace = \case
  "t" -> Just InTypes
  "v" -> Just InTerms
  _ -> Nothing

-- | Render names in their namespaces as fields, each namespace before its
-- name.
renderNamespaced :: Set (Namespace, OpName) -> [Text]
renderNamespaced names =
  concat [[renderNamespace namespace, op] | (namespace, OpName op) <- Set.toAscList names]

-- | Parse what 'renderNamespaced' rendered.
parseNamespaced :: [Text] -> Maybe [(Namespace, OpName)]
parseNamespaced = \case
  [] -> Just []
  namespace : op : rest ->
    (:) <$> ((,OpName op) <$> parseNamespace namespace) <*> parseNamespaced rest
  _ -> Nothing

-- | Where the cache keeps what the path names, or 'Nothing' if it keeps
-- nothing.
at :: Cache -> [Text] -> Maybe FilePath
at cache path = case cache of
  Cache root _ -> Just (foldl (</>) root (fmap T.unpack path))
  NoCache -> Nothing

-- | The token a cache is kept under.
tokenOf :: Cache -> Text
tokenOf = \case
  Cache _ (PlanToken token) -> token
  NoCache -> ""

-- | Read and parse a file, or 'Nothing' where there is none to read.
readIfPresent :: Maybe FilePath -> (Text -> a) -> IO (Maybe a)
readIfPresent place parse = case place of
  Nothing -> pure Nothing
  Just path -> quietly Nothing $ do
    there <- doesFileExist path
    if there then Just . parse <$> T.readFile path else pure Nothing

-- | Write via a temporary file and a rename, making the directory if need
-- be.
writeAtomically :: Maybe FilePath -> Text -> IO ()
writeAtomically place contents = case place of
  Nothing -> pure ()
  Just path -> quietly () $ do
    createDirectoryIfMissing True (takeDirectory path)
    let temporary = path <> ".tmp"
    T.writeFile temporary contents
    renameFile temporary path
