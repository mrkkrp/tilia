{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The whole fixity pipeline, run against this project's own dependencies.
--
-- These tests read the real build plan, the real package cache and real
-- Hackage sources. That is the point: every other test in the suite works
-- on constructed inputs, and constructed inputs are exactly what a pipeline
-- that talks to the outside world will not fail on.
--
-- Running the test suite implies the project was built, so the plan and the
-- sources are there. Where they are not — a sandboxed build with no package
-- cache — each test says so and is marked pending rather than failing.
module Tilia.Fixity.PlanSpec (spec) where

import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Fixity
import Tilia.Fixity.Plan
import Tilia.Parser

spec :: Spec
spec = do
  plan <- runIO (readBuildPlan (planPathFor "."))
  case plan of
    Left _ -> unavailable "no build plan; run cabal build first"
    Right p -> withPlan p

withPlan :: BuildPlan -> Spec
withPlan plan = do
  resolve <- runIO (newResolver plan)

  describe "the plan itself" $ do
    it "names the compiler" $
      T.unpack (bpCompiler plan) `shouldSatisfy` isInfixOf "ghc-"

    it "has the dependencies a real project has" $
      length (bpPackages plan) `shouldSatisfy` (> 20)

    it "gives every fetchable package a source hash to check against" $ do
      let fetchable = filter isFetchable (bpPackages plan)
      filter (null . sourceHashOf) fetchable `shouldBe` []

    it "does not mark the project itself as fetchable" $ do
      let locals = filter (\p -> ppName p == "tilia") (bpPackages plan)
      filter isFetchable locals `shouldBe` []

    it "records the project as a local directory, with its path" $ do
      let locals = [s' | p <- bpPackages plan, ppName p == "tilia", let s' = ppSource p]
      locals `shouldSatisfy` all (\s' -> case s' of LocalPackage path -> not (null path); _ -> False)

    it "puts every package in exactly one of the three kinds" $ do
      let kinds p = length (filter id [isPreExisting p, isFetchable p, isLocal p])
          isLocal p = case ppSource p of LocalPackage _ -> True; _ -> False
      filter ((/= 1) . kinds) (bpPackages plan) `shouldBe` []

  describe "resolving a module that declares its own operators" $ do
    it "finds <+> in prettyprinter, with the right fixity" $
      needs resolve "Prettyprinter.Internal" $ \fixities ->
        Map.lookup (OpName "<+>") fixities `shouldBe` Just (Fixity RightAssoc 6)

    it "resolves the same module twice to the same answer" $
      needs resolve "Prettyprinter.Internal" $ \first' -> do
        again <- resolve "Prettyprinter.Internal"
        again `shouldBe` Just first'

  describe "re-exports" $
    it "finds an operator a module exports but does not declare" $
      -- Prettyprinter re-exports <+> from Prettyprinter.Internal, where the
      -- infixr 6 actually lives.
      needs resolve "Prettyprinter" $ \fixities ->
        Map.lookup (OpName "<+>") fixities `shouldBe` Just (Fixity RightAssoc 6)

  describe "boot packages" $ do
    it "answers for Prelude from the built-in table" $
      needs resolve "Prelude" $ \fixities -> do
        Map.lookup (OpName "$") fixities `shouldBe` Just (Fixity RightAssoc 0)
        Map.lookup (OpName ">>=") fixities `shouldBe` Just (Fixity LeftAssoc 1)
        Map.lookup (OpName ".") fixities `shouldBe` Just (Fixity RightAssoc 9)
        Map.lookup (OpName ":") fixities `shouldBe` Just (Fixity RightAssoc 5)

    it "answers for Control.Applicative" $
      needs resolve "Control.Applicative" $ \fixities ->
        Map.lookup (OpName "<|>") fixities `shouldBe` Just (Fixity LeftAssoc 3)

    it "covers the containers and text modules a project actually imports" $ do
      let expected =
            [ ("Data.Map", "!", Fixity LeftAssoc 9),
              ("Data.Map", "\\\\", Fixity LeftAssoc 9),
              ("Data.Set", "\\\\", Fixity LeftAssoc 9),
              ("Data.Sequence", "|>", Fixity LeftAssoc 5),
              ("Data.Sequence", "<|", Fixity RightAssoc 5),
              ("Data.Bits", ".&.", Fixity LeftAssoc 7),
              ("Data.Ratio", "%", Fixity LeftAssoc 7),
              ("Data.Functor", "<&>", Fixity LeftAssoc 1),
              ("Control.Monad", ">=>", Fixity RightAssoc 1),
              ("Data.Semigroup", "<>", Fixity RightAssoc 6)
            ]
      wrong <- traverse (check resolve) expected
      concat wrong `shouldBe` []

    it "carries re-exports already resolved" $ do
      -- ($) is declared in an internal module and only reaches Prelude by
      -- re-export; (!) likewise reaches Data.Map from Data.Map.Internal.
      -- Both are in the table, so no chasing happens at run time.
      p <- resolve "Prelude"
      m <- resolve "Data.Map"
      ( Map.lookup (OpName "$") =<< p,
        Map.lookup (OpName "!") =<< m
        )
        `shouldBe` (Just (Fixity RightAssoc 0), Just (Fixity LeftAssoc 9))

    it "gives the same operator different fixities in different modules" $ do
      -- (\\\\) is infix 5 in Data.List and infixl 9 in Data.Map. A table keyed
      -- by operator rather than by module could not say this.
      inList <- resolve "Data.List"
      inMap <- resolve "Data.Map"
      ( Map.lookup (OpName "\\\\") =<< inList,
        Map.lookup (OpName "\\\\") =<< inMap
        )
        `shouldBe` (Just (Fixity NoAssoc 5), Just (Fixity LeftAssoc 9))

  describe "modules it cannot answer for" $ do
    it "says so rather than claiming no operators" $
      resolve "Not.A.Real.Module.At.All" `shouldReturn` Nothing

    it "says so for a module no package exposes" $
      resolve "Some.Package.That.Does.Not.Exist" `shouldReturn` Nothing

    it "distinguishes a boot module with no operators from an unknown one" $ do
      -- Data.Char exports none, and saying so is an answer. Were it merely
      -- absent from the table, importing it would leave every operator in
      -- scope unresolved.
      quiet <- resolve "Data.Char"
      quiet `shouldBe` Just Map.empty

  describe "the whole pipeline, from source text to a fixity" $ do
    it "resolves an operator through a real import" $
      endToEnd resolve "module M where\nimport Prettyprinter\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "prefers the module's own declaration to an imported one" $
      endToEnd resolve "module M where\nimport Prettyprinter\ninfixl 2 <+>\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity LeftAssoc 2) DeclaredHere

    it "honours a qualified import" $
      endToEnd resolve "module M where\nimport qualified Prettyprinter as P\n" $ \scope -> do
        lookupFixity scope (Just "P") (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")
        -- Qualified-only, so nothing arrives unqualified.
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "honours an explicit import list" $
      endToEnd resolve "module M where\nimport Prettyprinter ((<+>))\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "honours a hiding list" $
      endToEnd resolve "module M where\nimport Prettyprinter hiding ((<+>))\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "concludes the Report default when everything in scope was read" $
      endToEnd resolve "module M where\nimport Prettyprinter\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<!@#>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude anything when an import could not be read" $
      endToEnd resolve "module M where\nimport No.Such.Module\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<!@#>")
          `shouldBe` Unresolved ["No.Such.Module"]

    it "still answers for what it did find, despite an unreadable import" $
      endToEnd resolve "module M where\nimport Prettyprinter\nimport No.Such.Module\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<+>")
          `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prettyprinter")

    it "concludes the default through a boot import that exports no operators" $
      -- The table knowing Data.Char is what makes this a conclusion rather
      -- than an admission.
      endToEnd resolve "module M where\nimport Data.Char\n" $ \scope ->
        lookupFixity scope Nothing (OpName "<!@#>")
          `shouldBe` Resolved defaultFixity ReportDefault

    it "resolves an operator imported from a boot package" $
      endToEnd resolve "module M where\nimport Data.Map\n" $ \scope ->
        lookupFixity scope Nothing (OpName "!")
          `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "reports no ambiguity for a module that compiles" $
      endToEnd resolve "module M where\nimport Prettyprinter\n" $ \scope ->
        scopeAmbiguous scope `shouldBe` []

  describe "readiness" $ do
    it "reports something other than a missing plan for this project" $ do
      readiness <- checkReadiness "."
      readiness `shouldNotBe` PlanMissing

    it "reports a missing plan for a directory that has none" $
      checkReadiness "/" `shouldReturn` PlanMissing

----------------------------------------------------------------------------
-- Helpers

-- | Run an assertion on a module's fixities, or mark the test pending if
-- the module could not be resolved at all.
--
-- Pending rather than failing, because an unpopulated package cache is an
-- environment problem and not a defect in the code under test.
needs ::
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  Text ->
  (Map OpName Fixity -> Expectation) ->
  Expectation
needs resolve modName assertion =
  resolve modName >>= \case
    Nothing -> pendingWith ("could not resolve " <> T.unpack modName)
    Just fixities -> assertion fixities

-- | Parse a module, resolve its imports for real, and hand over the scope.
endToEnd ::
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  Text ->
  (Scope -> Expectation) ->
  Expectation
endToEnd resolve source assertion =
  case parseModule defaultParserConfig "test.hs" source of
    Left _ -> expectationFailure "the test input did not parse"
    Right pm -> do
      scope <- scopeFor resolve (pmModule pm)
      assertion scope

-- | Check one expected fixity, returning a description of any mismatch.
check ::
  (Text -> IO (Maybe (Map OpName Fixity))) ->
  (Text, Text, Fixity) ->
  IO [String]
check resolve (modName, op, expected) = do
  got <- resolve modName
  let actual = Map.lookup (OpName op) =<< got
  pure
    [ T.unpack modName <> "." <> T.unpack op
        <> ": expected "
        <> show expected
        <> " but got "
        <> show actual
    | actual /= Just expected
    ]

-- | Say why nothing could be tested, once, instead of failing repeatedly.
unavailable :: String -> Spec
unavailable reason =
  it "needs a built project" $ pendingWith reason
