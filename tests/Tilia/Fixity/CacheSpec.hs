{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The on-disk cache of what was read out of a package.
module Tilia.Fixity.CacheSpec (spec) where

import Data.Choice (pattern Do, pattern Don't)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
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

    describe "what reading a module established" $ do
      it "round-trips every direction" $ \cache -> do
        let established =
              declaring
                [ ((InTerms, OpName "<+>"), Fixity LeftAssoc 6),
                  ((InTerms, OpName ">>="), Fixity RightAssoc 1),
                  ((InTerms, OpName "==="), Fixity NoAssoc 4)
                ]
        storeEstablished cache "thing-1.0" "A.B" established
        cachedEstablished cache "thing-1.0" "A.B" `shouldReturn` Just established

      it "round-trips the extremes of precedence" $ \cache -> do
        let established =
              declaring
                [ ((InTerms, OpName "!"), Fixity LeftAssoc 0),
                  ((InTerms, OpName "?"), Fixity LeftAssoc 9),
                  ((InTerms, OpName "->"), Fixity RightAssoc (-1))
                ]
        storeEstablished cache "thing-1.0" "Edges" established
        cachedEstablished cache "thing-1.0" "Edges" `shouldReturn` Just established

      it "remembers that a module declares nothing" $ \cache -> do
        storeEstablished cache "thing-1.0" "Quiet" mempty
        cachedEstablished cache "thing-1.0" "Quiet" `shouldReturn` Just mempty

      it "remembers that a module could not be read" $ \cache -> do
        storeEstablished cache "thing-1.0" "Opaque" unreadable
        cachedEstablished cache "thing-1.0" "Opaque" `shouldReturn` Just unreadable

      it "remembers the way down to where reading gave up" $ \cache -> do
        storeEstablished cache "thing-1.0" "Opaque" (stoppedAt ["Middle", "Deep.Down"])
        cachedEstablished cache "thing-1.0" "Opaque"
          `shouldReturn` Just (stoppedAt ["Middle", "Deep.Down"])

      it "tells one stopped below it from one stopped on its own account" $ \cache -> do
        storeEstablished cache "thing-1.0" "Blamed" (stoppedAt ["Deep.Down"])
        storeEstablished cache "thing-1.0" "Itself" unreadable
        blamed <- cachedEstablished cache "thing-1.0" "Blamed"
        itself <- cachedEstablished cache "thing-1.0" "Itself"
        (blamed, itself) `shouldBe` (Just (stoppedAt ["Deep.Down"]), Just unreadable)

      it "round-trips the names it leaves unsettled, by namespace and way down" $ \cache -> do
        let established =
              mempty
                { establishedUnsettled =
                    Map.fromList
                      [ (["Deep.Down"], Set.fromList [(InTypes, OpName ":+:"), (InTerms, OpName "===")]),
                        ([], Set.singleton (InTerms, OpName "<+>"))
                      ]
                }
        storeEstablished cache "thing-1.0" "M" established
        cachedEstablished cache "thing-1.0" "M" `shouldReturn` Just established

      it "tells an unread module from one it was never told about" $ \cache -> do
        storeEstablished cache "thing-1.0" "Opaque" unreadable
        unread <- cachedEstablished cache "thing-1.0" "Opaque"
        never <- cachedEstablished cache "thing-1.0" "Absent"
        (unread, never) `shouldBe` (Just unreadable, Nothing)

      it "tells an unread module from one that declares nothing" $ \cache -> do
        storeEstablished cache "thing-1.0" "Opaque" unreadable
        storeEstablished cache "thing-1.0" "Quiet" mempty
        opaque <- cachedEstablished cache "thing-1.0" "Opaque"
        quiet <- cachedEstablished cache "thing-1.0" "Quiet"
        (opaque, quiet) `shouldBe` (Just unreadable, Just mempty)

      it "replaces an unread answer once the module can be read" $ \cache -> do
        storeEstablished cache "thing-1.0" "M" unreadable
        storeEstablished cache "thing-1.0" "M" (declaring [((InTerms, OpName "!"), Fixity LeftAssoc 9)])
        cachedEstablished cache "thing-1.0" "M"
          `shouldReturn` Just (declaring [((InTerms, OpName "!"), Fixity LeftAssoc 9)])

      it "knows nothing about a module it was never told about" $ \cache ->
        cachedEstablished cache "thing-1.0" "Absent" `shouldReturn` Nothing

      it "keeps packages apart" $ \cache -> do
        let ops = declaring [((InTerms, OpName "<>"), Fixity RightAssoc 6)]
        storeEstablished cache "a-1.0" "M" ops
        storeEstablished cache "b-1.0" "M" mempty
        a <- cachedEstablished cache "a-1.0" "M"
        b <- cachedEstablished cache "b-1.0" "M"
        (a, b) `shouldBe` (Just ops, Just mempty)

      it "treats a different hash in the key as a different package" $ \cache -> do
        storeEstablished cache "thing-1.0-aaaa" "M" (declaring [((InTerms, OpName "!"), Fixity LeftAssoc 9)])
        cachedEstablished cache "thing-1.0-bbbb" "M" `shouldReturn` Nothing

      it "overwrites a previous answer for the same key" $ \cache -> do
        storeEstablished cache "thing-1.0" "M" (declaring [((InTerms, OpName "!"), Fixity LeftAssoc 9)])
        storeEstablished cache "thing-1.0" "M" (declaring [((InTerms, OpName "!"), Fixity RightAssoc 3)])
        cachedEstablished cache "thing-1.0" "M"
          `shouldReturn` Just (declaring [((InTerms, OpName "!"), Fixity RightAssoc 3)])

      it "round-trips the members of each name, a name without members among them" $ \cache -> do
        let established =
              mempty
                { establishedMembers =
                    Map.fromList
                      [ (OpName "Empty", Set.empty),
                        (OpName "NonEmpty", Set.fromList [OpName ":|"]),
                        (OpName "Seq", Set.fromList [OpName ":<|", OpName ":|>"])
                      ]
                }
        storeEstablished cache "thing-1.0" "M" established
        cachedEstablished cache "thing-1.0" "M" `shouldReturn` Just established

      it "round-trips what it certainly brings in and the members of its types, by namespace" $ \cache -> do
        let established =
              mempty
                { establishedCertain =
                    Certain
                      ( Set.fromList
                          [ (InTypes, OpName "T"),
                            (InTerms, OpName "T"),
                            (InTerms, OpName ":|"),
                            (InTypes, OpName "Empty")
                          ]
                      )
                      ( Map.fromList
                          [ (OpName "T", Set.fromList [(InTerms, OpName "T"), (InTypes, OpName "F")]),
                            (OpName "Empty", Set.empty)
                          ]
                      ),
                  establishedMembers =
                    Map.fromList
                      [ (OpName "T", Set.fromList [OpName "T", OpName "F"]),
                        (OpName "Empty", Set.empty)
                      ]
                }
        storeEstablished cache "thing-1.0" "M" established
        cachedEstablished cache "thing-1.0" "M" `shouldReturn` Just established

      it "round-trips the members of a name beyond its certain members" $ \cache -> do
        let established =
              mempty
                { establishedCertain =
                    Certain
                      (Set.fromList [(InTypes, OpName "T"), (InTerms, OpName "A")])
                      (Map.singleton (OpName "T") (Set.singleton (InTerms, OpName "A"))),
                  establishedMembers =
                    Map.fromList
                      [ (OpName "T", Set.fromList [OpName "A", OpName "B"]),
                        (OpName "U", Set.empty)
                      ]
                }
        storeEstablished cache "thing-1.0" "M" established
        cachedEstablished cache "thing-1.0" "M" `shouldReturn` Just established

      it "round-trips all of it at once" $ \cache -> do
        let established =
              Established
                { establishedFixities = Map.singleton (InTerms, OpName "<+>") (Fixity LeftAssoc 6),
                  establishedUnsettled = Map.singleton ["Below"] (Set.singleton (InTerms, OpName "==>")),
                  establishedUntold = Set.fromList [["Below"], ["Other", "Further"]],
                  establishedCertain =
                    Certain
                      (Set.fromList [(InTerms, OpName "<+>"), (InTerms, OpName "==>")])
                      Map.empty,
                  establishedMembers = Map.singleton (OpName "T") (Set.singleton (OpName "A"))
                }
        storeEstablished cache "thing-1.0" "M" established
        cachedEstablished cache "thing-1.0" "M" `shouldReturn` Just established

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
        storeEstablished cache "thing-1.0" "A.B.C.D" (declaring [((InTerms, OpName "%"), Fixity NoAssoc 5)])
        cachedEstablished cache "thing-1.0" "A.B.C.D"
          `shouldReturn` Just (declaring [((InTerms, OpName "%"), Fixity NoAssoc 5)])

-- | A summary with something of every kind in it.
crowded :: ModuleSummary
crowded =
  ModuleSummary
    { summaryName = Just "M.N",
      summaryExports =
        Just
          [ ExportName InTerms Nothing (OpName "<+>"),
            ExportName InTerms (Just "Q") (OpName "<->"),
            ExportName InTypes Nothing (OpName ":|:"),
            ExportAll Nothing (OpName "T"),
            ExportAll (Just "Q") (OpName "U"),
            ExportSome Nothing (OpName "C") [OpName "method", OpName "F"],
            ExportSome (Just "Q") (OpName "V") [],
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
      summaryNames =
        Set.fromList
          [ (InTerms, OpName "<+>"),
            (InTypes, OpName "T"),
            (InTerms, OpName "T"),
            (InTypes, OpName ":|:")
          ],
      summaryDeclaredMembers =
        Map.fromList
          [ (OpName "T", Set.fromList [(InTerms, OpName "A"), (InTerms, OpName ":|:")]),
            (OpName "C", Set.fromList [(InTerms, OpName "method"), (InTypes, OpName "F")]),
            (OpName "Empty", Set.empty)
          ],
      summaryListedMembers = Map.fromList [(OpName "T", Set.singleton (OpName "A"))]
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

  it "carries what a package exposes that another one holds" $ \cache ->
    withDatabase $ \db -> do
      storeInstalled cache (Installed [prim] [db])
      cachedInstalled cache `shouldReturn` Just [prim]
  where
    containers =
      InstalledPackage
        { ipName = "containers",
          ipVersion = "0.7",
          ipModules = ["Data.Map", "Data.Map.Strict", "Data.Set"],
          ipReexports = [],
          ipImportDirs = ["/nowhere/containers-0.7"]
        }
    prim =
      InstalledPackage
        { ipName = "ghc-prim",
          ipVersion = "0.13.1",
          ipModules = [],
          ipReexports =
            [ ("GHC.Prim", "GHC.Internal.Prim"),
              ("GHC.Types", "GHC.Internal.Types")
            ],
          ipImportDirs = ["/nowhere/ghc-prim-0.13.1"]
        }
    quiet =
      InstalledPackage
        { ipName = "rts",
          ipVersion = "1.0",
          ipModules = [],
          ipReexports = [],
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

-- | What an answer that leaves names unsettled is tied to, and what it is
-- not.
tokens :: Spec
tokens = around withIsolatedDirectory $ do
  it "does not offer an unread answer written under another token" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeEstablished before' "thing-1.0" "M" unreadable
    after' <- open dir (PlanToken "two")
    cachedEstablished after' "thing-1.0" "M" `shouldReturn` Nothing

  it "does not offer one that names what it leaves unsettled either" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeEstablished
      before'
      "thing-1.0"
      "M"
      mempty{establishedUnsettled = Map.singleton ["Below"] (Set.singleton (InTerms, OpName "==>"))}
    after' <- open dir (PlanToken "two")
    cachedEstablished after' "thing-1.0" "M" `shouldReturn` Nothing

  it "still offers one written under the same token" $ \dir -> do
    before' <- open dir (PlanToken "one")
    storeEstablished before' "thing-1.0" "M" unreadable
    again <- open dir (PlanToken "one")
    cachedEstablished again "thing-1.0" "M" `shouldReturn` Just unreadable

  it "keeps an answer that settles everything, whatever the token" $ \dir -> do
    let established = declaring [((InTerms, OpName "<+>"), Fixity RightAssoc 6)]
    before' <- open dir (PlanToken "one")
    storeEstablished before' "thing-1.0" "M" established
    after' <- open dir (PlanToken "two")
    cachedEstablished after' "thing-1.0" "M" `shouldReturn` Just established

-- | What reading a module that settles every name and declares these
-- fixities established.
declaring :: [((Namespace, OpName), Fixity)] -> Established
declaring fixities = mempty{establishedFixities = Map.fromList fixities}

-- | What reading a module that gave up on the way down given established.
stoppedAt :: [Text] -> Established
stoppedAt chain = mempty{establishedUntold = Set.singleton chain}

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
