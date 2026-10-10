{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

-- | The fixity machinery, run over every dependency this project has.
--
-- "Tilia.Fixity.PlanSpec" checks the pipeline on a handful of modules
-- picked for what each one exercises. This checks it on all of them. Every
-- module of every package in this project's build plan is read out of the
-- package's source tarball and compared against what the compiler recorded
-- when it built that same package: two independent readings of one fact,
-- one by us and one by GHC.
module Tilia.Fixity.DependenciesSpec (spec) where

import Control.Monad (filterM)
import Data.ByteString qualified as BS
import Data.Choice (pattern Do, pattern Don't, pattern Is)
import Data.Foldable (for_)
import Data.List (isSuffixOf, sort)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import Tilia.BootFixities (bootFixities)
import Tilia.Cpp (blankCpp)
import Tilia.Fixity
import Tilia.Fixity.Cache (PlanToken (..), openCache)
import Tilia.Fixity.Interface (Interface (..), readInterface)
import Tilia.Fixity.PackageDb
import Tilia.Fixity.Plan
import Tilia.Gathered (gathered)
import Tilia.Parser
import Tilia.WithProjectPlan
  ( Dependency (..),
    compilerShipped,
    dependenciesOf,
    testsFor,
    withProjectPlan,
  )

spec :: Spec
spec = withProjectPlan withPlan

withPlan :: BuildPlan -> Spec
withPlan plan = do
  installed <- runIO readInstalledPackages
  fromSource <- runIO (fixitiesThrough <$> newResolverVia (Do #useCache) [FromSource] plan)
  fromInterface <- runIO (fixitiesThrough <$> newResolverVia (Do #useCache) [FromInterface] plan)
  resolver <- runIO (newResolver plan)
  let resolve = fixitiesThrough resolver
  own <- runIO ownModules
  let isShippedModule m = Map.member m bootFixities
  dependencies <- runIO (dependenciesOf (not . isShippedModule) plan installed)
  preloaded <- runIO (dependenciesOf isShippedModule plan installed)
  shippedPackages <- runIO compilerShipped
  let modules = concatMap depModules dependencies
      readFromSourcePackages =
        Set.fromList (fmap depPackage dependencies)
          `Set.difference` shippedPackages
  noCache <- runIO (openCache (Don't #useCache) (PlanToken ""))
  missing <-
    runIO $
      filterM (fmap not . doesFileExist . snd)
        . filter ((`Set.member` readFromSourcePackages) . ppName . fst)
        =<< plannedTarballs noCache plan
  describe "the tree this project is built against" $ do
    it "is a real dependency tree and not an empty plan" $
      length dependencies `shouldSatisfy` (>= 30)

    it "holds modules the compiler does not ship a fixity table for" $
      length modules `shouldSatisfy` (>= 500)

    -- What every comparison below reads one of its two sides out of. A
    -- source that is not here contradicts nothing, so the comparisons would
    -- pass without having compared anything: this is where that is caught,
    -- rather than in a hundred quietly hollow ticks.
    it "has the source of every package it reads" $
      case fmap (T.unpack . ppName . fst) missing of
        [] -> pure ()
        names ->
          expectationFailure $
            "no source for "
              <> unwords names
              <> "; the test suite fetches these itself, so something stopped it"

  describe "the operators the compiler ships with" $
    parallel $
      for_ (concatMap testsFor preloaded) $ \(label, chunk) ->
        it label $ do
          wrong <- traverse contradicts chunk
          concat wrong `shouldBe` []

  describe "every dependency declares what the compiler recorded" $
    parallel $
      for_ (concatMap testsFor dependencies) $ \(label, chunk) ->
        it label $ do
          wrong <- traverse (undeclared fromSource) chunk
          concat wrong `shouldBe` []

  describe "every dependency reads the same both ways" $
    parallel $
      for_ (concatMap testsFor dependencies) $ \(label, chunk) ->
        it label $ do
          wrong <- traverse (conflicting fromSource fromInterface . fst) chunk
          concat wrong `shouldBe` []

  describe "how much of the tree it reaches" $ do
    it "answers for every module of every dependency" $ do
      answers <- traverse (\(m, _) -> (m,) <$> resolve m) modules
      [m | (m, Nothing) <- answers] `shouldBe` []

    it "answers for every one of them out of the interfaces alone" $ do
      answers <- traverse (\(m, _) -> (m,) <$> fromInterface m) modules
      [m | (m, Nothing) <- answers] `shouldBe` []

    -- A loose floor on purpose. How much of the tree source alone reaches
    -- depends on the order the modules are asked for: a module in a
    -- re-export cycle is answered with what the cycle held when the chase
    -- reached it, and which module of the cycle gives way is whichever was
    -- entered first. Measured over ghc-lib-parser's 450 modules, sweeping
    -- them forwards, backwards and from twelve threads moved two of them
    -- either way, and over the whole tree, the boot packages read from
    -- source too, the figure has been seen at 93% on GHC 9.10. The check is
    -- here to catch the route collapsing, not to pin a number that is not
    -- pinned.
    it "reads most of them out of source alone" $ do
      answers <- traverse (fromSource . fst) modules
      let reached = length [() | Just _ <- answers]
      percent reached (length modules) `shouldSatisfy` (>= 70)

    it "finds the operators that are in it" $ do
      answers <- traverse (fromSource . fst) modules
      sum [Map.size fixities | Just fixities <- answers] `shouldSatisfy` (>= 300)

  describe "this project's own modules" $ do
    it "parses every one of them" $
      [path | (path, Nothing) <- own] `shouldBe` []

    it "resolves every module they import" $ do
      answers <- traverse (\m -> (m,) <$> resolve m) (importedByOwn own)
      [m | (m, Nothing) <- answers] `shouldBe` []

    it "settles every operator they use" $ do
      unsettled <- traverse (unsettledIn resolver) [(path, m) | (path, Just m) <- own]
      concat unsettled `shouldBe` []

----------------------------------------------------------------------------
-- The dependencies

-- | Where the boot table and the compiler both hold a fixity for an
-- operator and it is not the same fixity.
--
-- The table in "Tilia.BootFixities" was written by asking a GHC of one
-- version what its boot packages export. The tests run against whichever
-- GHC built the project, which this package supports three of. This is what
-- says the answer has not moved underneath the table.
contradicts :: (Text, FilePath) -> IO [String]
contradicts (modName, interfaceFile) =
  readInterface modName interfaceFile >>= \case
    Nothing -> pure []
    Just interface ->
      pure
        [ T.unpack modName
            <> ": "
            <> show op
            <> " is "
            <> show declared
            <> " per the compiler, "
            <> show ours
            <> " in the table"
        | (op, declared) <- Map.toList (interfaceDeclares interface),
          Just ours <- [Map.lookup op table],
          ours /= declared
        ]
  where
    table = Map.findWithDefault Map.empty modName bootFixities

-- | Every fixity the compiler recorded for a module that reading the
-- package's source did not produce.
undeclared ::
  -- | What a module declares, read from the package's source.
  (Text -> IO (Maybe (Fixities))) ->
  -- | The module, and the interface the compiler wrote for it.
  (Text, FilePath) ->
  IO [String]
undeclared fromSource (modName, interfaceFile) =
  readInterface modName interfaceFile >>= \case
    Nothing -> pure []
    Just interface ->
      fromSource modName >>= \case
        Nothing -> pure []
        Just fixities ->
          pure
            [ T.unpack modName
                <> ": "
                <> show op
                <> " is "
                <> show declared
                <> " per the compiler, "
                <> show (Map.lookup op fixities)
                <> " from source"
            | (op, declared) <- Map.toList (interfaceDeclares interface),
              writable declared,
              Map.lookup op fixities /= Just declared
            ]
  where
    -- GHC files @->@ under a module's fixities at precedence -1, below
    -- anything a source file is allowed to declare.
    writable f = fixityPrecedence f >= 0 && fixityPrecedence f <= 9

-- | Where the two routes both have an answer for an operator and it is not
-- the same answer.
--
-- Wider than 'undeclared', because a module's own declarations are the
-- smaller part of what it offers: most operators reach the module that
-- exports them through a chain of re-exports, and following that chain
-- through source text is the part of this most likely to go wrong. The
-- compiler followed the same chain when it built the package, so the two
-- have to arrive at the same place.
conflicting ::
  -- | The answer read out of the package's source.
  (Text -> IO (Maybe (Fixities))) ->
  -- | The answer read out of the compiler's interfaces.
  (Text -> IO (Maybe (Fixities))) ->
  Text ->
  IO [String]
conflicting fromSource fromInterface modName = do
  source <- fromSource modName
  compiled <- fromInterface modName
  pure $ case (source, compiled) of
    (Just a, Just b) ->
      [ T.unpack modName
          <> ": "
          <> show op
          <> " is "
          <> show fromText
          <> " from source, "
          <> show fromIface
          <> " from the interface"
      | (op, (fromText, fromIface)) <- Map.toList (Map.intersectionWith (,) a b),
        fromText /= fromIface
      ]
    _ -> []

percent :: Int -> Int -> Int
percent part whole = if whole == 0 then 0 else part * 100 `div` whole

----------------------------------------------------------------------------
-- This project

-- | Every Haskell file this project is made of, parsed.
--
-- 'Nothing' where one did not parse, which is a failure of its own rather
-- than something to skip over quietly.
ownModules :: IO [(FilePath, Maybe (HsModule GhcPs))]
ownModules = do
  paths <- concat <$> traverse haskellFilesIn ["src", "app", "tests"]
  traverse parsed paths
  where
    parsed path = do
      source <- T.decodeUtf8Lenient <$> BS.readFile path
      pure
        ( path,
          case parseModule defaultParserConfig path (blankCpp source) of
            Left _ -> Nothing
            Right pm -> Just (pmModule pm)
        )

haskellFilesIn :: FilePath -> IO [FilePath]
haskellFilesIn dir = do
  entries <- sort <$> listDirectory dir
  concat <$> traverse below entries
  where
    below entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then haskellFilesIn path
        else pure [path | ".hs" `isSuffixOf` path]

-- | What a module exports, where reading it settles every name.
fixitiesThrough :: Resolver -> Text -> IO (Maybe Fixities)
fixitiesThrough resolver modName = settled <$> askModule resolver modName
  where
    settled established
      | settlesEverything established = Just (establishedFixities established)
      | otherwise = Nothing

-- | Every module this project's own source imports.
importedByOwn :: [(FilePath, Maybe (HsModule GhcPs))] -> [Text]
importedByOwn own =
  Set.toList . Set.fromList $
    [ importModule i
    | (_, Just hsModule) <- own,
      i <- moduleImports (Is #implicitPrelude) (pure hsModule),
      not ("Paths_" `T.isPrefixOf` importModule i)
    ]

-- | The operators one of this project's modules uses that its imports,
-- resolved for real, cannot settle.
--
-- This is the whole machinery end to end: the plan is read, the packages
-- are found, their modules are read, the scope is assembled and the
-- operators are looked up in it. Anything left over is an operator this
-- project could not be laid out from.
unsettledIn ::
  Resolver ->
  (FilePath, HsModule GhcPs) ->
  IO [String]
unsettledIn resolver (path, hsModule) = do
  scope <- scopeFor resolver (Is #implicitPrelude) (pure hsModule)
  pure
    [ path <> ": " <> T.unpack (operatorSpelling qualifier op) <> " " <> show why
    | ((qualifier, op), why) <- unknownOperators scope (pure (gathered hsModule))
    ]
