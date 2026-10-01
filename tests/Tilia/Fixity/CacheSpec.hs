{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The on-disk cache of what was read out of a package.
module Tilia.Fixity.CacheSpec (spec) where

import Data.Choice (pattern Do, pattern Don't)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import System.Directory (getModificationTime, listDirectory, setModificationTime)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Fixity
import Tilia.Fixity.Cache
import Tilia.Fixity.PackageDb (Installed (..), InstalledPackage (..))

spec :: Spec
spec = do
  tokens
  database
  describe "a cache not to be used" $
    it "remembers nothing, and nothing is made on disk for it" $
      withIsolatedDirectory $ \dir -> do
        setEnv "XDG_CACHE_HOME" dir
        cache <- openCache (Don't #useCache) (PlanToken "plan")
        unsetEnv "XDG_CACHE_HOME"
        storeModules cache "thing-1.0" ["A"]
        cachedModules cache "thing-1.0" `shouldReturn` Nothing
        listDirectory dir `shouldReturn` []
  around withIsolatedCache $ do
    describe "modules" $ do
      it "remembers a package's module list" $ \cache -> do
        storeModules cache "thing-1.0-abc" ["A.B", "C"]
        cachedModules cache "thing-1.0-abc" `shouldReturn` Just ["A.B", "C"]

      it "knows nothing about a package it was never told about" $ \cache ->
        cachedModules cache "absent-1.0" `shouldReturn` Nothing

      it "remembers an empty list as a fact, not as absence" $ \cache -> do
        storeModules cache "empty-1.0" []
        cachedModules cache "empty-1.0" `shouldReturn` Just []

    describe "fixities" $ do
      it "round-trips every direction" $ \cache -> do
        let fixities =
              Map.fromList
                [ ((InTerms, OpName "<+>"), Fixity LeftAssoc 6),
                  ((InTerms, OpName ">>="), Fixity RightAssoc 1),
                  ((InTerms, OpName "==="), Fixity NoAssoc 4)
                ]
        storeFixities cache "thing-1.0" "A.B" (Declares fixities)
        cachedFixities cache "thing-1.0" "A.B" `shouldReturn` Just (Declares fixities)

      it "round-trips the extremes of precedence" $ \cache -> do
        let fixities =
              Map.fromList
                [ ((InTerms, OpName "!"), Fixity LeftAssoc 0),
                  ((InTerms, OpName "?"), Fixity LeftAssoc 9),
                  ((InTerms, OpName "->"), Fixity RightAssoc (-1))
                ]
        storeFixities cache "thing-1.0" "Edges" (Declares fixities)
        cachedFixities cache "thing-1.0" "Edges" `shouldReturn` Just (Declares fixities)

      it "remembers that a module declares nothing" $ \cache -> do
        storeFixities cache "thing-1.0" "Quiet" (Declares Map.empty)
        cachedFixities cache "thing-1.0" "Quiet" `shouldReturn` Just (Declares Map.empty)

      it "remembers that a module could not be read" $ \cache -> do
        storeFixities cache "thing-1.0" "Opaque" (Unreadable Nothing)
        cachedFixities cache "thing-1.0" "Opaque" `shouldReturn` Just (Unreadable Nothing)

      it "remembers which module below it stopped the reading" $ \cache -> do
        storeFixities cache "thing-1.0" "Opaque" (Unreadable (Just "Deep.Down"))
        cachedFixities cache "thing-1.0" "Opaque"
          `shouldReturn` Just (Unreadable (Just "Deep.Down"))

      it "tells one stopped below it from one stopped on its own account" $ \cache -> do
        storeFixities cache "thing-1.0" "Blamed" (Unreadable (Just "Deep.Down"))
        storeFixities cache "thing-1.0" "Itself" (Unreadable Nothing)
        blamed <- cachedFixities cache "thing-1.0" "Blamed"
        itself <- cachedFixities cache "thing-1.0" "Itself"
        (blamed, itself)
          `shouldBe` (Just (Unreadable (Just "Deep.Down")), Just (Unreadable Nothing))

      it "tells an unread module from one it was never told about" $ \cache -> do
        storeFixities cache "thing-1.0" "Opaque" (Unreadable Nothing)
        unread <- cachedFixities cache "thing-1.0" "Opaque"
        never <- cachedFixities cache "thing-1.0" "Absent"
        (unread, never) `shouldBe` (Just (Unreadable Nothing), Nothing)

      it "tells an unread module from one that declares nothing" $ \cache -> do
        storeFixities cache "thing-1.0" "Opaque" (Unreadable Nothing)
        storeFixities cache "thing-1.0" "Quiet" (Declares Map.empty)
        opaque <- cachedFixities cache "thing-1.0" "Opaque"
        quiet <- cachedFixities cache "thing-1.0" "Quiet"
        (opaque, quiet) `shouldBe` (Just (Unreadable Nothing), Just (Declares Map.empty))

      it "replaces an unread answer once the module can be read" $ \cache -> do
        storeFixities cache "thing-1.0" "M" (Unreadable Nothing)
        storeFixities cache "thing-1.0" "M" (Declares (Map.fromList [((InTerms, OpName "!"), Fixity LeftAssoc 9)]))
        cachedFixities cache "thing-1.0" "M"
          `shouldReturn` Just (Declares (Map.fromList [((InTerms, OpName "!"), Fixity LeftAssoc 9)]))

      it "knows nothing about a module it was never told about" $ \cache ->
        cachedFixities cache "thing-1.0" "Absent" `shouldReturn` Nothing

      it "keeps packages apart" $ \cache -> do
        let ops = Map.fromList [((InTerms, OpName "<>"), Fixity RightAssoc 6)]
        storeFixities cache "a-1.0" "M" (Declares ops)
        storeFixities cache "b-1.0" "M" (Declares Map.empty)
        a <- cachedFixities cache "a-1.0" "M"
        b <- cachedFixities cache "b-1.0" "M"
        (a, b) `shouldBe` (Just (Declares ops), Just (Declares Map.empty))

      it "treats a different hash in the key as a different package" $ \cache -> do
        storeFixities cache "thing-1.0-aaaa" "M" (Declares (Map.fromList [((InTerms, OpName "!"), Fixity LeftAssoc 9)]))
        cachedFixities cache "thing-1.0-bbbb" "M" `shouldReturn` Nothing

      it "overwrites a previous answer for the same key" $ \cache -> do
        storeFixities cache "thing-1.0" "M" (Declares (Map.fromList [((InTerms, OpName "!"), Fixity LeftAssoc 9)]))
        storeFixities cache "thing-1.0" "M" (Declares (Map.fromList [((InTerms, OpName "!"), Fixity RightAssoc 3)]))
        cachedFixities cache "thing-1.0" "M"
          `shouldReturn` Just (Declares (Map.fromList [((InTerms, OpName "!"), Fixity RightAssoc 3)]))

    describe "export names" $ do
      it "round-trips the names an export list gave" $ \cache -> do
        let names = Just (Set.fromList [OpName "<+>", OpName ":|", OpName "f"])
        storeExportNames cache "thing-1.0" "M" names
        cachedExportNames cache "thing-1.0" "M" `shouldReturn` Just names

      it "remembers a module that keeps its own counsel" $ \cache -> do
        storeExportNames cache "thing-1.0" "M" Nothing
        cachedExportNames cache "thing-1.0" "M" `shouldReturn` Just Nothing

      it "tells one that keeps its own counsel from one never asked about" $ \cache -> do
        storeExportNames cache "thing-1.0" "Quiet" Nothing
        cachedExportNames cache "thing-1.0" "Quiet" `shouldReturn` Just Nothing
        cachedExportNames cache "thing-1.0" "Unasked" `shouldReturn` Nothing

      it "tells one that exports nothing from one that will not say" $ \cache -> do
        storeExportNames cache "thing-1.0" "Bare" (Just Set.empty)
        storeExportNames cache "thing-1.0" "Quiet" Nothing
        cachedExportNames cache "thing-1.0" "Bare"
          `shouldReturn` Just (Just Set.empty)
        cachedExportNames cache "thing-1.0" "Quiet" `shouldReturn` Just Nothing

      it "keeps packages apart" $ \cache -> do
        storeExportNames cache "one-1.0" "M" (Just (Set.singleton (OpName "<+>")))
        storeExportNames cache "two-1.0" "M" (Just (Set.singleton (OpName "<?>")))
        cachedExportNames cache "one-1.0" "M"
          `shouldReturn` Just (Just (Set.singleton (OpName "<+>")))

      it "keeps them apart from the fixities of the same module" $ \cache -> do
        storeFixities cache "thing-1.0" "M" (Unreadable Nothing)
        storeExportNames cache "thing-1.0" "M" (Just (Set.singleton (OpName "<+>")))
        cachedFixities cache "thing-1.0" "M" `shouldReturn` Just (Unreadable Nothing)
        cachedExportNames cache "thing-1.0" "M"
          `shouldReturn` Just (Just (Set.singleton (OpName "<+>")))

      it "overwrites a previous answer for the same key" $ \cache -> do
        storeExportNames cache "thing-1.0" "M" Nothing
        storeExportNames cache "thing-1.0" "M" (Just (Set.singleton (OpName "<+>")))
        cachedExportNames cache "thing-1.0" "M"
          `shouldReturn` Just (Just (Set.singleton (OpName "<+>")))

    describe "what a name carries with it" $ do
      it "round-trips what each name carries" $ \cache -> do
        let kept =
              Map.fromList
                [ (OpName "NonEmpty", Set.fromList [OpName ":|"]),
                  (OpName "Seq", Set.fromList [OpName ":<|", OpName ":|>"])
                ]
        storeChildren cache "thing-1.0" "M" kept
        cachedChildren cache "thing-1.0" "M" `shouldReturn` Just kept

      it "remembers a module that carries nothing anywhere" $ \cache -> do
        storeChildren cache "thing-1.0" "Bare" Map.empty
        cachedChildren cache "thing-1.0" "Bare" `shouldReturn` Just Map.empty

      it "tells that from a module it was never told about" $ \cache -> do
        storeChildren cache "thing-1.0" "Bare" Map.empty
        cachedChildren cache "thing-1.0" "Unasked" `shouldReturn` Nothing

      it "remembers a name that carries nothing among ones that do" $ \cache -> do
        let kept =
              Map.fromList
                [ (OpName "Empty", Set.empty),
                  (OpName "NonEmpty", Set.singleton (OpName ":|"))
                ]
        storeChildren cache "thing-1.0" "M" kept
        cachedChildren cache "thing-1.0" "M" `shouldReturn` Just kept

      it "keeps packages apart" $ \cache -> do
        storeChildren cache "one-1.0" "M" (Map.singleton (OpName "T") (Set.singleton (OpName ":|")))
        cachedChildren cache "two-1.0" "M" `shouldReturn` Nothing

    describe "what the project's own modules say" $ do
      it "round-trips every kind of thing a summary holds" $ \cache -> do
        let summaries = Just (crowded :| [bare, unlisted])
        storeSummaries cache "m" "stamp" summaries
        cachedSummaries cache "m" "stamp" `shouldReturn` Just summaries

      it "remembers a module none of whose configurations parsed" $ \cache -> do
        storeSummaries cache "m" "stamp" Nothing
        cachedSummaries cache "m" "stamp" `shouldReturn` Just Nothing

      it "gives back nothing for what it was not read from" $ \cache -> do
        storeSummaries cache "m" "before" (Just (crowded :| []))
        cachedSummaries cache "m" "after" `shouldReturn` Nothing

      it "keeps only what the latest text of a module said" $ \cache -> do
        storeSummaries cache "m" "before" (Just (crowded :| []))
        storeSummaries cache "m" "after" (Just (bare :| []))
        cachedSummaries cache "m" "before" `shouldReturn` Nothing
        cachedSummaries cache "m" "after" `shouldReturn` Just (Just (bare :| []))

      it "keeps modules apart" $ \cache -> do
        storeSummaries cache "m" "stamp" (Just (crowded :| []))
        cachedSummaries cache "n" "stamp" `shouldReturn` Nothing

    describe "module names with dots" $
      it "files a deeply qualified module without confusion" $ \cache -> do
        storeFixities cache "thing-1.0" "A.B.C.D" (Declares (Map.fromList [((InTerms, OpName "%"), Fixity NoAssoc 5)]))
        cachedFixities cache "thing-1.0" "A.B.C.D"
          `shouldReturn` Just (Declares (Map.fromList [((InTerms, OpName "%"), Fixity NoAssoc 5)]))

-- | A summary with something of every kind in it.
crowded :: ModuleSummary
crowded =
  ModuleSummary
    { summaryName = Just "M.N",
      summaryExports =
        Just
          [ ExportName Nothing (OpName "<+>"),
            ExportName (Just "Q") (OpName "<->"),
            ExportAll Nothing (OpName "T"),
            ExportAll (Just "Q") (OpName "U"),
            ExportModule "Data.List"
          ],
      summaryImports =
        [ Import "Prelude" False "Prelude" Nothing,
          Import "Data.List" True "Q" Nothing,
          Import
            "Data.Map"
            False
            "Data.Map"
            ( Just
                ( False,
                  [ ImportedName (OpName "!"),
                    ImportedAll (OpName "Map"),
                    ImportedSome (OpName "NonEmpty") [OpName ":|", OpName "toList"]
                  ]
                )
            ),
          Import "Data.Set" False "S" (Just (True, [ImportedName (OpName "\\\\")])),
          Import "Data.Void" False "Data.Void" (Just (False, []))
        ],
      summaryFixities =
        Map.fromList
          [ ((InTerms, OpName "<+>"), Fixity LeftAssoc 6),
            ((InTypes, OpName ":|:"), Fixity RightAssoc 5),
            ((InTerms, OpName "div"), Fixity NoAssoc 7)
          ],
      summaryNames = Set.fromList [OpName "<+>", OpName "T", OpName ":|:"],
      summaryDeclaredChildren =
        Map.fromList
          [ (OpName "T", Set.fromList [OpName "A", OpName ":|:"]),
            (OpName "Empty", Set.empty)
          ],
      summaryChildren = Map.fromList [(OpName "T", Set.singleton (OpName "A"))]
    }

-- | A summary with nothing in it, not even a name.
bare :: ModuleSummary
bare = ModuleSummary Nothing Nothing [] Map.empty Set.empty Map.empty Map.empty

-- | One whose export list lists nothing, which is not the same as having
-- no export list.
unlisted :: ModuleSummary
unlisted = bare{summaryName = Just "M", summaryExports = Just []}

-- | What the compiler can see, and what it takes to stop believing it.
--
-- The database stands in for @ghc-pkg@ here: what is under test is that a
-- change to it is noticed, not what @ghc-pkg@ would have said about it.
database :: Spec
database = around withIsolatedCache $ do
  it "gives back what it was told, while the database sits still" $ \cache ->
    withDatabase $ \db -> do
      storeInstalled cache (Installed [containers] [db])
      cachedInstalled cache `shouldReturn` Just [containers]

  it "gives back nothing once a package has been registered" $ \cache ->
    withDatabase $ \db -> do
      storeInstalled cache (Installed [containers] [db])
      writeFile (db </> "new-1.0.conf") ""
      cachedInstalled cache `shouldReturn` Nothing

  it "gives back nothing once the database is gone" $ \cache -> do
    db <- withDatabase pure
    storeInstalled cache (Installed [containers] [db])
    cachedInstalled cache `shouldReturn` Nothing

  it "remembers nothing it has no way to stop believing" $ \cache -> do
    storeInstalled cache (Installed [containers] [])
    cachedInstalled cache `shouldReturn` Nothing

  it "gives back nothing to a token it was not written under" $ \_ ->
    withIsolatedDirectory $ \dir ->
      withDatabase $ \db -> do
        before' <- open dir (PlanToken "one")
        storeInstalled before' (Installed [containers] [db])
        after' <- open dir (PlanToken "two")
        cachedInstalled after' `shouldReturn` Nothing

  it "gives it back under the token it was written under" $ \_ ->
    withIsolatedDirectory $ \dir ->
      withDatabase $ \db -> do
        before' <- open dir (PlanToken "one")
        storeInstalled before' (Installed [containers] [db])
        again <- open dir (PlanToken "one")
        cachedInstalled again `shouldReturn` Just [containers]

  it "keeps one token's answer when another writes its own" $ \_ ->
    withIsolatedDirectory $ \dir ->
      withDatabase $ \db -> do
        one <- open dir (PlanToken "one")
        storeInstalled one (Installed [containers] [db])
        two <- open dir (PlanToken "two")
        storeInstalled two (Installed [quiet] [db])
        cachedInstalled one `shouldReturn` Just [containers]
        cachedInstalled two `shouldReturn` Just [quiet]

  it "carries a package that exposes nothing" $ \cache ->
    withDatabase $ \db -> do
      storeInstalled cache (Installed [containers, quiet] [db])
      cachedInstalled cache `shouldReturn` Just [containers, quiet]
  where
    containers =
      InstalledPackage
        { ipName = "containers",
          ipVersion = "0.7",
          ipModules = ["Data.Map", "Data.Map.Strict", "Data.Set"],
          ipImportDirs = ["/nowhere/containers-0.7"]
        }
    quiet =
      InstalledPackage
        { ipName = "rts",
          ipVersion = "1.0",
          ipModules = [],
          ipImportDirs = []
        }

-- | A directory standing in for a package database, with a timestamp that
-- can be set rather than waited for.
withDatabase :: (FilePath -> IO a) -> IO a
withDatabase action =
  withSystemTempDirectory "tilia-db" $ \db -> do
    -- Something long ago, so that anything happening to the directory
    -- afterwards is a change whatever the clock's resolution.
    setModificationTime db =<< getModificationTime "/"
    action db

-- | What an answer of \"could not be read\" is tied to, and what it is not.
tokens :: Spec
tokens = around withIsolatedDirectory $ do
  it "does not offer an unread answer written under another token" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeFixities before' "thing-1.0" "M" (Unreadable Nothing)
    after' <- open dir (PlanToken "two")
    cachedFixities after' "thing-1.0" "M" `shouldReturn` Nothing

  it "still offers one written under the same token" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeFixities before' "thing-1.0" "M" (Unreadable Nothing)
    again <- open dir (PlanToken "one")
    cachedFixities again "thing-1.0" "M" `shouldReturn` Just (Unreadable Nothing)

  it "keeps an answer that was read, whatever the token" $ \dir -> do
    let fixities = Map.fromList [((InTerms, OpName "<+>"), Fixity RightAssoc 6)]
    before' <- open dir (PlanToken "one")
    storeFixities before' "thing-1.0" "M" (Declares fixities)
    after' <- open dir (PlanToken "two")
    cachedFixities after' "thing-1.0" "M" `shouldReturn` Just (Declares fixities)

  it "keeps what an export list said, whatever the token" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeExportNames before' "thing-1.0" "M" Nothing
    after' <- open dir (PlanToken "two")
    cachedExportNames after' "thing-1.0" "M" `shouldReturn` Just Nothing

-- | Give each test its own cache directory, so nothing leaks between them
-- or into the developer's real cache.
withIsolatedCache :: (Cache -> IO ()) -> IO ()
withIsolatedCache action =
  withIsolatedDirectory (\dir -> open dir (PlanToken "plan") >>= action)

withIsolatedDirectory :: (FilePath -> IO ()) -> IO ()
withIsolatedDirectory = withSystemTempDirectory "tilia-cache"

-- | Open a cache in a given directory, under a given token.
open :: FilePath -> PlanToken -> IO Cache
open dir token = do
  setEnv "XDG_CACHE_HOME" dir
  cache <- openCache (Do #useCache) token
  unsetEnv "XDG_CACHE_HOME"
  pure cache
