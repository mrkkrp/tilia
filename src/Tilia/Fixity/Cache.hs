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
    cachedEstablished,
    storeEstablished,
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

import Control.Monad (guard, join)
import Data.Choice (Choice, isFalse)
import Data.Foldable (toList, traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
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
cachedEstablished ::
  -- | Where to look.
  Cache ->
  -- | The package the module belongs to. Opaque here.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  -- | What was established, or 'Nothing' if nothing was, or if what it
  -- left unsettled was left so under another plan.
  IO (Maybe Established)
cachedEstablished cache package modName =
  fmap join . readIfPresent (at cache ["established", package, modName]) $ \contents ->
    case T.lines contents of
      ("established" : entries) -> assembled (fmap (T.splitOn "\t") entries)
      _ -> Nothing
  where
    assembled entries = do
      guard (all (`elem` ["fixity", "unsettled", "untold", "names", "member", "members"]) (concatMap (take 1) entries))
      fixities <- traverse (parseFixity . T.intercalate "\t") [fields | "fixity" : fields <- entries]
      unsettled <- traverse unsettledEntry [fields | "unsettled" : fields <- entries]
      untold <- traverse untoldEntry [fields | "untold" : fields <- entries]
      names <- concat <$> traverse parseByNamespace [fields | "names" : fields <- entries]
      certain <- traverse (memberEntry parseByNamespace) [fields | "member" : fields <- entries]
      members <- traverse (memberEntry (Just . fmap OpName)) [fields | "members" : fields <- entries]
      pure
        Established
          { establishedFixities = Map.fromList fixities,
            establishedUnsettled = Map.fromListWith Set.union unsettled,
            establishedUntold = Set.fromList untold,
            establishedCertain = Certain (Set.fromList names) (Map.fromListWith Set.union certain),
            establishedMembers =
              Map.union (Map.fromListWith Set.union members) (Map.fromListWith Set.union (fmap (fmap (Set.map snd)) certain))
          }
    unsettledEntry = \case
      token : chain : names
        | token == tokenOf cache -> (T.words chain,) . Set.fromList <$> parseNamespaced names
      _ -> Nothing
    untoldEntry = \case
      [token, chain] | token == tokenOf cache -> Just (T.words chain)
      _ -> Nothing
    memberEntry kidsOf = \case
      parent : kids -> (OpName parent,) . Set.fromList <$> kidsOf kids
      [] -> Nothing

-- | Remember what reading a module established.
storeEstablished ::
  -- | Where to write.
  Cache ->
  -- | The package the module belongs to, as 'cachedEstablished' takes it.
  Text ->
  -- | The module, by its full dotted name.
  Text ->
  -- | What was established about it.
  Established ->
  IO ()
storeEstablished cache package modName established =
  writeAtomically (at cache ["established", package, modName]) . T.unlines $
    "established"
      : fmap (("fixity\t" <>) . renderFixity) (Map.toList (establishedFixities established))
        <> [ T.intercalate "\t" (["unsettled", tokenOf cache, T.unwords chain] <> renderNamespaced names)
           | (chain, names) <- Map.toList (establishedUnsettled established)
           ]
        <> [ T.intercalate "\t" ["untold", tokenOf cache, T.unwords chain]
           | chain <- Set.toList (establishedUntold established)
           ]
        <> [T.intercalate "\t" ("names" : fields) | fields <- renderByNamespace (certainNames certain)]
        <> [ T.intercalate "\t" ("member" : parent : fields)
           | (OpName parent, kids) <- Map.toList (certainMembers certain),
             fields <- case renderByNamespace kids of
               [] -> [[]]
               grouped -> grouped
           ]
        -- The members of a name are mostly its certain members, which the
        -- lines above already say.
        <> [ T.intercalate "\t" ("members" : parent : [kid | OpName kid <- Set.toAscList kids])
           | (OpName parent, kids) <- Map.toList (establishedMembers established),
             Just kids /= fmap (Set.map snd) (Map.lookup (OpName parent) (certainMembers certain))
           ]
  where
    certain = establishedCertain established

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
        <> fmap declares (Map.toList (summaryDeclaredMembers s))
        <> fmap offers (Map.toList (summaryListedMembers s))
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
        go s{summaryDeclaredMembers = Map.insert (OpName parent) (Set.fromList namespaced) (summaryDeclaredMembers s)} rest
      ("offers" : parent : kids) : rest ->
        go s{summaryListedMembers = Map.insert (OpName parent) (names kids) (summaryListedMembers s)} rest
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
          then traverse installedFrom [l | ("pkg" : l) <- fmap fields ls]
          else Nothing
  where
    unchanged (path, stamp) =
      quietly False ((== stamp) . T.pack . show <$> getModificationTime path)
    installedFrom = \case
      name : version : modules : reexports : dirs -> do
        pairs <- traverse reexport (T.words reexports)
        pure
          InstalledPackage
            { ipName = name,
              ipVersion = version,
              ipModules = T.words modules,
              ipReexports = pairs,
              ipImportDirs = fmap T.unpack dirs
            }
      _ -> Nothing
    reexport pair = case T.splitOn "=" pair of
      [visible, original] -> Just (visible, original)
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
                 [ "pkg",
                   ipName p,
                   ipVersion p,
                   T.unwords (ipModules p),
                   T.unwords
                     [ visible <> "=" <> original
                     | (visible, original) <- ipReexports p
                     ]
                 ]
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

-- | Render names as one list of fields for each namespace they are in, the
-- namespace first.
renderByNamespace :: Set (Namespace, OpName) -> [[Text]]
renderByNamespace names =
  [ renderNamespace namespace : ops
  | namespace <- [InTypes, InTerms],
    let ops = [op | (n, OpName op) <- Set.toAscList names, n == namespace],
    not (null ops)
  ]

-- | Parse one list of fields 'renderByNamespace' rendered.
parseByNamespace :: [Text] -> Maybe [(Namespace, OpName)]
parseByNamespace = \case
  [] -> Just []
  namespace : ops -> (\n -> fmap ((n,) . OpName) ops) <$> parseNamespace namespace

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
