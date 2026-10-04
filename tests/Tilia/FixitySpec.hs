{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Whether fixities can be resolved exactly from source alone.
module Tilia.FixitySpec (spec) where

import Data.Choice (Choice, pattern Is, pattern Isn't)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import Test.Hspec
import Tilia.Cpp (blankCpp, branchLeaves)
import Tilia.Fixity
import Tilia.Parser
import Tilia.Span (spanStartColumn, spanStartLine)

spec :: Spec
spec = do
  describe "layer 1: what a module declares" $ do
    it "reads a left-associative declaration" $
      declaredIn "module M where\ninfixl 6 <+>\n"
        `shouldBe` [(OpName "<+>", Fixity LeftAssoc 6)]

    it "reads a right-associative declaration" $
      declaredIn "module M where\ninfixr 5 <>>\n"
        `shouldBe` [(OpName "<>>", Fixity RightAssoc 5)]

    it "reads a non-associative declaration" $
      declaredIn "module M where\ninfix 4 ===\n"
        `shouldBe` [(OpName "===", Fixity NoAssoc 4)]

    it "reads several operators from one declaration" $
      declaredIn "module M where\ninfixl 7 <.>, <:>\n"
        `shouldBe` [(OpName "<.>", Fixity LeftAssoc 7), (OpName "<:>", Fixity LeftAssoc 7)]

    it "reads a backticked function name" $
      declaredIn "module M where\ninfixl 7 `quot`\n"
        `shouldBe` [(OpName "quot", Fixity LeftAssoc 7)]

    it "finds nothing when nothing is declared" $
      declaredIn "module M where\nx = 1\n" `shouldBe` []

    it "reads a declaration that appears after its use" $
      declaredIn "module M where\ny = a <+> b\ninfixl 6 <+>\n"
        `shouldBe` [(OpName "<+>", Fixity LeftAssoc 6)]

    it "reads one a class makes about its own method" $
      declaredIn "module M where\nclass C a where\n  infixr 8 .=\n  (.=) :: a -> a -> Int\n"
        `shouldBe` [(OpName ".=", Fixity RightAssoc 8)]

    it "reads those at the margin and in a class together" $
      declaredIn "module M where\ninfixl 1 <+>\nclass C a where\n  infixr 8 .=\n  (.=) :: a -> a -> Int\n"
        `shouldBe` [(OpName ".=", Fixity RightAssoc 8), (OpName "<+>", Fixity LeftAssoc 1)]

    it "leaves a declaration local to a binding where it is" $
      declaredIn "module M where\nf = g\n  where\n    infixr 3 ###\n    g = 1\n"
        `shouldBe` []

  describe "which namespace a declaration governs" $ do
    it "gives a type operator's fixity to types" $
      declaredWithNamespaces "module M where\ninfixr 4 :>\ndata a :> b = Sub a b\n"
        `shouldBe` [((InTypes, OpName ":>"), Fixity RightAssoc 4)]

    it "gives a value operator's fixity to terms" $
      declaredWithNamespaces "module M where\ninfixl 6 <+>\na <+> b = a\n"
        `shouldBe` [((InTerms, OpName "<+>"), Fixity LeftAssoc 6)]

    it "gives a pattern synonym's fixity to terms" $
      declaredWithNamespaces
        "{-# LANGUAGE PatternSynonyms #-}\nmodule M where\ninfixl 5 :>\npattern x :> y = (x, y)\n"
        `shouldBe` [((InTerms, OpName ":>"), Fixity LeftAssoc 5)]

    it "honours a declaration that names the type namespace itself" $
      declaredWithNamespaces "module M where\ninfixr 4 type :>\n"
        `shouldBe` [((InTypes, OpName ":>"), Fixity RightAssoc 4)]

    it "honours one that names the data namespace" $
      declaredWithNamespaces "module M where\ninfixr 4 data :>\n"
        `shouldBe` [((InTerms, OpName ":>"), Fixity RightAssoc 4)]

    it "gives both to a name the module declares in neither" $
      declaredWithNamespaces "module M where\ninfixr 4 <?>\n"
        `shouldBe` [ ((InTypes, OpName "<?>"), Fixity RightAssoc 4),
                     ((InTerms, OpName "<?>"), Fixity RightAssoc 4)
                   ]

    it "gives both to a name the module declares in both" $
      declaredWithNamespaces "module M where\ninfixr 4 :>\ndata a :> b = a :> b\n"
        `shouldBe` [ ((InTypes, OpName ":>"), Fixity RightAssoc 4),
                     ((InTerms, OpName ":>"), Fixity RightAssoc 4)
                   ]

    it "gives a class's fixity to types and its method's to terms" $
      declaredWithNamespaces
        "module M where\ninfixl 3 <%>\nclass a <%> b where\n  infixl 7 .=\n  (.=) :: a -> b -> Int\n"
        `shouldBe` [ ((InTypes, OpName "<%>"), Fixity LeftAssoc 3),
                     ((InTerms, OpName ".="), Fixity LeftAssoc 7)
                   ]

  describe "one spelling in two namespaces" $ do
    it "settles a type use against the type declaration" $
      lookupFixity (scopeOfBoth "import Types\nimport Terms\n") InTypes Nothing (OpName ":>")
        `shouldBe` Resolved (Fixity RightAssoc 4) (DeclaredIn "Types")

    it "settles a term use against the term declaration" $
      lookupFixity (scopeOfBoth "import Types\nimport Terms\n") InTerms Nothing (OpName ":>")
        `shouldBe` Resolved (Fixity LeftAssoc 5) (DeclaredIn "Terms")

    it "reports no ambiguity between them" $ do
      let scope = scopeOfBoth "import Types\nimport Terms\n"
      reachAmbiguous (scopeInTypes scope) `shouldBe` Map.empty
      reachAmbiguous (scopeInTerms scope) `shouldBe` Map.empty

    it "still reports one where both are in the same namespace" $
      reachAmbiguous (scopeInTerms (scopeOfBoth "import Terms\nimport Other.Terms\n"))
        `shouldBe` Map.fromList [((Nothing, OpName ":>"), termsDisagreeing)]

    it "takes a promoted constructor's fixity from the terms" $
      lookupFixity (scopeOfBoth "import Terms\n") InTypes Nothing (OpName ":>")
        `shouldBe` Resolved (Fixity LeftAssoc 5) (DeclaredIn "Terms")

    it "does not take a type's fixity for a term" $
      lookupFixity (scopeOfBoth "import Types\n") InTerms Nothing (OpName ":>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "prefers the type it finds to the term it could fall back on" $
      lookupFixity (scopeOfBoth "import Types\nimport Terms\n") InTypes Nothing (OpName ":>")
        `shouldBe` Resolved (Fixity RightAssoc 4) (DeclaredIn "Types")

    it "lets a module that writes the type be formatted" $
      unsettledIn "module M where\nimport Types\nimport Terms\ntype T = Int :> Int\n"
        `shouldBe` []

    it "declines one that writes an operator both agree to disagree about" $
      fmap snd (unsettledIn "module M where\nimport Terms\nimport Other.Terms\nf a b = a :> b\n")
        `shouldBe` [Ambiguous termsDisagreeing]

  describe "what a module says it exports" $ do
    it "has nothing to say about a module with no export list" $
      exportsOfSource "module M where\nf = 1\n" `shouldBe` Nothing

    it "keeps the qualifier a name was written under" $
      exportsOfSource "module M ((Disp.<+>)) where\n"
        `shouldBe` Just [ExportName InTerms (Just "Disp") (OpName "<+>")]

    it "has none for a name written plainly" $
      exportsOfSource "module M ((<+>)) where\n"
        `shouldBe` Just [ExportName InTerms Nothing (OpName "<+>")]

    it "reads a whole module passed on as the module it names" $
      exportsOfSource "module M (module Data.Map) where\n"
        `shouldBe` Just [ExportModule "Data.Map"]

  describe "which unread module an unsettled operator is blamed on" $ do
    it "passes over one whose export list has no such operator" $
      lookupFixity (scopeKnowing [("Opaque", ["<+>"])] usesUnknown) InTerms Nothing (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "blames one whose export list names it" $
      lookupFixity (scopeKnowing [("Opaque", ["<??>"])] usesUnknown) InTerms Nothing (OpName "<??>")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "blames one that will not say what it exports" $
      lookupFixity (scopeKnowing [] usesUnknown) InTerms Nothing (OpName "<??>")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "still passes over an import list that does not name it" $
      lookupFixity
        (scopeKnowing [("Opaque", ["<??>"])] "module M where\nimport Opaque ((<+>))\n")
        InTerms
        Nothing
        (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "blames only the ones that could supply it, of several unread" $
      lookupFixity
        (scopeKnowing [("Opaque", ["<+>"]), ("Other.Opaque", ["<??>"])] twoUnread)
        InTerms
        Nothing
        (OpName "<??>")
        `shouldBe` Unresolved (unreadOnly "Other.Opaque")

    it "settles nothing on its own account when told nothing" $
      lookupFixity (fullScope usesUnknown) InTerms Nothing (OpName "<??>")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "blames a module once, however many of its imports could supply it" $
      lookupFixity
        (fullScope "module M where\nimport Opaque\nimport Opaque ((<??>))\n")
        InTerms
        Nothing
        (OpName "<??>")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "passes over one whose import list names no such operator" $
      lookupFixity
        (scopeSuspecting [("Opaque", [("T", ["<+>"])])] "import Opaque (T (..))\n")
        InTerms
        Nothing
        (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "blames one whose import list names it" $
      lookupFixity
        (scopeSuspecting [("Opaque", [("T", ["<??>"])])] "import Opaque (T (..))\n")
        InTerms
        Nothing
        (OpName "<??>")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "blames one whose (..) nothing is known about" $
      lookupFixity
        (scopeSuspecting [] "import Opaque (T (..))\n")
        InTerms
        Nothing
        (OpName "<??>")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "passes over one that hides the operator along with its type" $
      lookupFixity
        (scopeSuspecting [("Opaque", [("T", ["<??>"])])] "import Opaque hiding (T (..))\n")
        InTerms
        Nothing
        (OpName "<??>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "lets a file be formatted when no unread module could have declared it" $
      unknownOperators
        (scopeKnowing [("Opaque", ["<+>"])] usesUnknown)
        (pure (pmGathered (parsed usesUnknown)))
        `shouldBe` []

    it "passes over every one for a name the module defines itself" $
      lookupFixity (fullScope definesItsOwn) InTerms Nothing (OpName "clamp")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "passes over every one for a use under the module's own name" $
      lookupFixity (fullScope definesItsOwn) InTerms (Just "M") (OpName "clamp")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "still blames one for a use under the qualifier it is imported as" $
      lookupFixity
        (fullScope "module M where\nimport qualified Opaque as O\nclamp = max\n")
        InTerms
        (Just "O")
        (OpName "clamp")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "still blames one for a type when the module defines only a constructor" $
      lookupFixity
        (fullScope "module M where\nimport Opaque\ndata T = Int :+: Int\n")
        InTypes
        Nothing
        (OpName ":+:")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "lets a file be formatted that uses what it defines itself" $
      unknownOperators (fullScope definesItsOwn) (pure (pmGathered (parsed definesItsOwn)))
        `shouldBe` []

    it "passes over every one for a name an import it could read brings in" $
      lookupFixity (scopeBringing preludeBringsMin "import Opaque\n") InTerms Nothing (OpName "min")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "passes over every one for a name brought in under the qualifier used" $
      lookupFixity
        (scopeBringing [("Data.List", [(InTerms, "isPrefixOf")])] "import qualified Data.List as L\nimport qualified Opaque as L\n")
        InTerms
        (Just "L")
        (OpName "isPrefixOf")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "still blames one for a name brought in only under another qualifier" $
      lookupFixity
        (scopeBringing [("Data.List", [(InTerms, "isPrefixOf")])] "import qualified Data.List as L\nimport Opaque\n")
        InTerms
        Nothing
        (OpName "isPrefixOf")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "passes over every one for a variable an import list names" $
      lookupFixity
        (scopeBringing [("Data.List", [(InTerms, "isPrefixOf")])] "import Data.List (isPrefixOf)\nimport Opaque\n")
        InTerms
        Nothing
        (OpName "isPrefixOf")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "still blames one for a name an import list leaves out" $
      lookupFixity
        (scopeBringing [("Data.List", [(InTerms, "isPrefixOf")])] "import Data.List (nub)\nimport Opaque\n")
        InTerms
        Nothing
        (OpName "isPrefixOf")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "still blames one for a name a hiding list may hide along with a type" $
      lookupFixity
        (scopeBringing [("Readable", [(InTerms, ":|")])] "import Readable hiding (T (..))\nimport Opaque\n")
        InTerms
        Nothing
        (OpName ":|")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "still blames one for a constructor brought in only as a type" $
      lookupFixity
        (scopeBringing [("Readable", [(InTypes, ":+:")])] "import Readable\nimport Opaque\n")
        InTerms
        Nothing
        (OpName ":+:")
        `shouldBe` Unresolved (unreadOnly "Opaque")

    it "does not take a name a module brings in for settled where it leaves the name unsettled" $
      let leavingMin = \case
            "Opaque" ->
              mempty
                { establishedCertain = Certain (Set.singleton (InTerms, OpName "min")) Map.empty,
                  establishedUnsettled = Map.singleton [] (Set.singleton (InTerms, OpName "min"))
                }
            m -> knowingExports m
          scope = resolveScope (Is #implicitPrelude) leavingMin (pure (pmModule (parsed "module M where\nimport Opaque\n")))
       in lookupFixity scope InTerms Nothing (OpName "min") `shouldBe` Unresolved (unreadOnly "Opaque")

    it "lets a file be formatted whose every unsettled name is accounted for" $
      let source =
            T.unlines
              [ "module C (w) where",
                "import A",
                "w :: Int",
                "w = (y `clamp` 2) `min` 3 + n `roundTo` 2",
                "  where",
                "    n = 7",
                "    a `roundTo` b = a * b",
                "clamp :: Int -> Int -> Int",
                "clamp = max"
              ]
          scope =
            resolveScope
              (Is #implicitPrelude)
              ( \case
                  "A" -> unreadable
                  "Prelude" ->
                    mempty
                      { establishedFixities =
                          inBothNamespaces (Map.fromList [(OpName "+", Fixity LeftAssoc 6), (OpName "*", Fixity LeftAssoc 7)]),
                        establishedCertain =
                          Certain (Set.fromList [(InTerms, OpName "min"), (InTerms, OpName "+"), (InTerms, OpName "*")]) Map.empty
                      }
                  m -> knowingExports m
              )
              (pure (pmModule (parsed source)))
       in unknownOperators scope (pure (pmGathered (parsed source))) `shouldBe` []

  describe "a use a local binding captures" $ do
    it "is captured by a function its equation binds under where" $
      capturedIn "f = 1 `roundTo` 2\n  where\n    a `roundTo` b = a * b\n"
        `shouldBe` [((1, 7), defaultFixity)]

    it "takes the fixity the binding's group declares" $
      capturedIn "f = 1 <+> 2 <+> 3\n  where\n    infixr 5 <+>\n    a <+> b = a * b\n"
        `shouldBe` [((1, 7), Fixity RightAssoc 5), ((1, 13), Fixity RightAssoc 5)]

    it "is captured by what a let binds" $
      capturedIn "f = let a <+> b = a * b in 1 <+> 2\n"
        `shouldBe` [((1, 30), defaultFixity)]

    it "is captured by an argument of the equation" $
      capturedIn "f op = 1 `op` 2\n"
        `shouldBe` [((1, 10), defaultFixity)]

    it "is captured by an argument of a lambda" $
      capturedIn "f = \\op -> 1 `op` 2\n"
        `shouldBe` [((1, 14), defaultFixity)]

    it "is captured by the pattern of a case alternative" $
      capturedIn "f x = case x of\n  Just op -> 1 `op` 2\n"
        `shouldBe` [((2, 16), defaultFixity)]

    it "is captured by a pattern guard before it" $
      capturedIn "f x\n  | Just op <- x = 1 `op` 2\n"
        `shouldBe` [((2, 22), defaultFixity)]

    it "is captured by a bind earlier in a do block" $
      capturedIn "f = do\n  op <- get\n  pure (1 `op` 2)\n"
        `shouldBe` [((3, 11), defaultFixity)]

    it "is captured by a bind in a comprehension, before it as written" $
      capturedIn "f xs = [1 `op` 2 | op <- xs]\n"
        `shouldBe` [((1, 11), defaultFixity)]

    it "is not captured by a bind later in a do block" $
      capturedIn "f = do\n  x <- pure (1 `op` 2)\n  op <- get\n  pure x\n"
        `shouldBe` []

    it "is not captured outside the equation that binds it" $
      capturedIn "f = 1 `op` 2\ng = x\n  where\n    op = (+)\n"
        `shouldBe` []

    it "is not captured by a function the module defines at its top level" $
      capturedIn "f = 1 `op` 2\nop = (+)\n"
        `shouldBe` []

    it "is not captured by a method an instance defines" $
      capturedIn "instance Semigroup T where\n  a <> b = a\nf = a <> b\n"
        `shouldBe` []

    it "is not captured where bindings of different fixities could each capture it" $
      capturedIn "f op = let infixr 5 `op`\n           a `op` b = a in 1 `op` 2\n"
        `shouldBe` []

    it "lets a file be formatted that uses it alongside an unread import" $
      unsettledIn "module M where\nimport Opaque\nf = 1 `roundTo` 2\n  where\n    a `roundTo` b = a\n"
        `shouldBe` []

    it "is still held against the unread import where nothing binds it" $
      fmap fst (unsettledIn "module M where\nimport Opaque\nf = 1 `roundTo` 2\n")
        `shouldBe` [(Nothing, OpName "roundTo")]

  describe "the members of a name" $ do
    it "takes a type's constructors, among terms" $
      declaredMembersIn "module M where\ndata T = A | Int :| Int\n"
        `shouldBe` [(OpName "T", [(InTerms, OpName ":|"), (InTerms, OpName "A")])]

    it "takes a record's fields, which may be operators, among terms" $
      declaredMembersIn "module M where\ndata T = T {(#) :: Int, name :: Int}\n"
        `shouldBe` [(OpName "T", [(InTerms, OpName "#"), (InTerms, OpName "T"), (InTerms, OpName "name")])]

    it "takes a GADT's constructors, among terms" $
      declaredMembersIn "module M where\ndata T where\n  A :: T\n  (:|) :: T -> T\n"
        `shouldBe` [(OpName "T", [(InTerms, OpName ":|"), (InTerms, OpName "A")])]

    it "takes a class's methods, among terms" $
      declaredMembersIn "module M where\nclass C a where\n  (.=) :: a -> a -> Int\n  named :: a\n"
        `shouldBe` [(OpName "C", [(InTerms, OpName ".="), (InTerms, OpName "named")])]

    it "takes a class's associated families, among types" $
      declaredMembersIn "module M where\nclass C a where\n  type F a\n"
        `shouldBe` [(OpName "C", [(InTypes, OpName "F")])]

    it "has nothing to say about a type synonym" $
      declaredMembersIn "module M where\ntype T = Int\n" `shouldBe` []

    it "keeps to what an export list hands on" $
      listedMembersIn "module M (T (A)) where\ndata T = A | Int :| Int\n"
        `shouldBe` [(OpName "T", [OpName "A"])]

    it "hands on every member of a name exported with (..)" $
      listedMembersIn "module M (T (..)) where\ndata T = A | Int :| Int\n"
        `shouldBe` [(OpName "T", [OpName ":|", OpName "A"])]

    it "says nothing of the members of a type it does not declare" $
      listedMembersIn "module M (T (..)) where\nimport Elsewhere\n"
        `shouldBe` []

  describe "layer 2: imports" $ do
    it "brings in an operator among the members of a type" $
      lookupFixity (scopeWithMembers "import Members (T (..))\n") InTerms Nothing (OpName ":|")
        `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Members")

    it "leaves out an operator not among the members of the type" $
      lookupFixity (scopeWithMembers "import Members (T (..))\n") InTerms Nothing (OpName "<+>")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "hides an operator hidden along with its type" $
      lookupFixity (scopeWithMembers "import Members hiding (T (..))\n") InTerms Nothing (OpName ":|")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "keeps what a hiding list leaves alone" $
      lookupFixity (scopeWithMembers "import Members hiding (T (..))\n") InTerms Nothing (OpName "<+>")
        `shouldBe` Resolved (Fixity LeftAssoc 6) (DeclaredIn "Members")

    it "brings in one a T(..) may stand for, where its members are not known" $
      lookupFixity
        (fullScope "module M where\nimport Members (T (..))\n")
        InTerms
        Nothing
        (OpName ":|")
        `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Members")

    it "leaves out one no item of that list could name" $
      lookupFixity
        (fullScope "module M where\nimport Members (f)\n")
        InTerms
        Nothing
        (OpName ":|")
        `shouldBe` Resolved defaultFixity ReportDefault

    it "keeps one a hiding T(..) cannot be shown to have hidden" $
      lookupFixity
        (fullScope "module M where\nimport Members hiding (T (..))\n")
        InTerms
        Nothing
        (OpName ":|")
        `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Members")

    it "brings it in under a qualifier too" $
      lookupFixity
        (scopeWithMembers "import qualified Members as C (T (..))\n")
        InTerms
        (Just "C")
        (OpName ":|")
        `shouldBe` Resolved (Fixity RightAssoc 5) (DeclaredIn "Members")

    it "sees an unqualified import in both scopes" $
      scopeOf "module M where\nimport Data.Map\n"
        `shouldBe` ( [(OpName "!", Fixity LeftAssoc 9)],
                     [(("Data.Map", OpName "!"), Fixity LeftAssoc 9)],
                     []
                   )

    it "does not bring a qualified import into unqualified scope" $
      scopeOf "module M where\nimport qualified Data.Map\n"
        `shouldBe` ([], [(("Data.Map", OpName "!"), Fixity LeftAssoc 9)], [])

    it "makes an alias the qualifier" $
      scopeOf "module M where\nimport qualified Data.Map as M\n"
        `shouldBe` ([], [(("M", OpName "!"), Fixity LeftAssoc 9)], [])

    it "keeps unqualified names when an alias is not qualified" $
      scopeOf "module M where\nimport Data.Map as M\n"
        `shouldBe` ( [(OpName "!", Fixity LeftAssoc 9)],
                     [(("M", OpName "!"), Fixity LeftAssoc 9)],
                     []
                   )

    it "honours an explicit import list" $
      scopeOf "module M where\nimport Data.Sequence ((|>))\n"
        `shouldBe` ( [(OpName "|>", Fixity LeftAssoc 5)],
                     [(("Data.Sequence", OpName "|>"), Fixity LeftAssoc 5)],
                     []
                   )

    it "honours a hiding list" $
      scopeOf "module M where\nimport Data.Sequence hiding ((|>))\n"
        `shouldBe` ( [(OpName "<|", Fixity RightAssoc 5)],
                     [(("Data.Sequence", OpName "<|"), Fixity RightAssoc 5)],
                     []
                   )

    it "lets the module's own declaration win over an import" $
      let (unq, _, _) = scopeOf "module M where\nimport Data.Map\ninfixr 3 !\n"
       in unq `shouldBe` [(OpName "!", Fixity RightAssoc 3)]

  describe "ambiguity" $ do
    it "reports an operator imported with two different fixities" $
      let (_, _, amb) = scopeOf "module M where\nimport Data.Map\nimport Other\n"
       in amb `shouldBe` [(Nothing, OpName "!")]

    it "reports nothing when two imports agree" $
      let (_, _, amb) = scopeOf "module M where\nimport Data.Map\nimport Agreeing\n"
       in amb `shouldBe` []

    it "reports nothing when the two go under different names" $
      let (_, _, amb) =
            scopeOf "module M where\nimport Data.Map\nimport qualified Other\n"
       in amb `shouldBe` []

    it "reports an alias two imports disagree under" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport qualified Data.Map as M\nimport qualified Other as M\n"
       in amb `shouldBe` [(Just "M", OpName "!")]

    it "reports nothing when two imports under one alias agree" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport qualified Data.Map as M\nimport qualified Agreeing as M\n"
       in amb `shouldBe` []

    it "counts an unqualified import towards the name it goes under" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport Data.Map\nimport qualified Other as Data.Map\n"
       in amb `shouldBe` [(Just "Data.Map", OpName "!")]

    it "keeps a clashing alias apart from a clashing bare name" $
      let (_, _, amb) =
            scopeOf
              "module M where\nimport Data.Map\nimport Other\nimport qualified Data.Map as M\nimport qualified Other as M\n"
       in amb `shouldBe` [(Nothing, OpName "!"), (Just "M", OpName "!")]

  describe "a module compiled with NoImplicitPrelude" $ do
    it "settles an operator on the one module that is really in scope" $
      let s =
            scopeAboutPrelude (Isn't #implicitPrelude) takesItsPreludeElsewhere
       in lookupFixity s InTerms Nothing (OpName "<%>")
            `shouldBe` Resolved (Fixity LeftAssoc 6) (DeclaredIn "Pretty")

    it "is left with nothing unsettled" $
      unsettledAboutPrelude (Isn't #implicitPrelude) usingItBothWays
        `shouldBe` []

    it "would be caught between two spellings were the Prelude assumed" $
      fmap snd (unsettledAboutPrelude (Is #implicitPrelude) usingItBothWays)
        `shouldBe` [ Ambiguous
                       (("Prelude", Fixity RightAssoc 6) :| [("Pretty", Fixity LeftAssoc 6)])
                   ]

    it "still takes the Prelude where the module does import it" $
      let s =
            scopeAboutPrelude
              (Isn't #implicitPrelude)
              "module M where\nimport Prelude\n"
       in lookupFixity s InTerms Nothing (OpName "<%>")
            `shouldBe` Resolved (Fixity RightAssoc 6) (DeclaredIn "Prelude")

  describe "lookupFixity" $ do
    it "finds an unqualified operator" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s InTerms Nothing (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "finds a qualified operator through its alias" $
      let s = fullScope "module M where\nimport qualified Data.Map as M\n"
       in lookupFixity s InTerms (Just "M") (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "concludes infixl 9 when every module in scope was read" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s InTerms Nothing (OpName "<??>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude anything when a module could not be read" $
      let s = fullScope "module M where\nimport Data.Map\nimport Opaque\n"
       in lookupFixity s InTerms Nothing (OpName "<??>")
            `shouldBe` Unresolved (unreadOnly "Opaque")

    it "still answers for an operator it did find, despite an unread module" $
      let s = fullScope "module M where\nimport Data.Map\nimport Opaque\n"
       in lookupFixity s InTerms Nothing (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "attributes the module\'s own declaration to itself" $
      let s = fullScope "module M where\ninfixr 3 <+>\n"
       in lookupFixity s InTerms Nothing (OpName "<+>")
            `shouldBe` Resolved (Fixity RightAssoc 3) DeclaredHere

    it "takes the fixity of : from the language, whatever is in scope" $
      let s =
            scopeAboutPrelude
              (Isn't #implicitPrelude)
              "module M where\nimport Opaque\n"
          cons = Resolved (Fixity RightAssoc 5) BuiltIn
       in ( lookupFixity s InTerms Nothing (OpName ":"),
            lookupFixity s InTypes Nothing (OpName ":")
          )
            `shouldBe` (cons, cons)

    it "does not find a qualified-only operator unqualified" $
      let s = fullScope "module M where\nimport qualified Data.Map\n"
       in lookupFixity s InTerms Nothing (OpName "!")
            `shouldBe` Resolved defaultFixity ReportDefault

  describe "a qualified use is answered from qualified scope alone" $ do
    it "does not answer a qualifier that brought nothing in from what did" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s InTerms (Just "Q") (OpName "!")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "does not lend the module's own declaration to a foreign qualifier" $
      let s = fullScope "module M where\ninfixr 3 <+>\n"
       in lookupFixity s InTerms (Just "Q") (OpName "<+>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "does not answer through an alias the import does not go under" $
      let s = fullScope "module M where\nimport qualified Data.Map as M\n"
       in lookupFixity s InTerms (Just "Data.Map") (OpName "!")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "answers a use qualified by the module's own name" $
      let s = fullScope "module M where\ninfixr 3 <+>\n"
       in lookupFixity s InTerms (Just "M") (OpName "<+>")
            `shouldBe` Resolved (Fixity RightAssoc 3) DeclaredHere

    it "answers a plain import under the module's own name" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s InTerms (Just "Data.Map") (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "weighs only the unread imports the qualifier reaches" $
      let s = fullScope "module M where\nimport qualified Data.Map as M\nimport Opaque\n"
       in lookupFixity s InTerms (Just "M") (OpName "<??>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude when the qualifier reaches an unread import" $
      let s = fullScope "module M where\nimport qualified Opaque as O\n"
       in lookupFixity s InTerms (Just "O") (OpName "<??>")
            `shouldBe` Unresolved (unreadOnly "Opaque")

  describe "what could not be settled" $ do
    it "says which qualifier the unsettled use was written under" $
      unsettledIn "module M where\nimport qualified Opaque as O\nf a b = a O.<+> b\n"
        `shouldBe` [((Just "O", OpName "<+>"), NotRead (unreadOnly "Opaque"))]

    it "keeps a qualified use apart from an unqualified one" $
      unsettledIn
        "module M where\nimport Opaque\nimport qualified Opaque as O\nf a b = a <+> b\ng a b = a O.<+> b\n"
        `shouldBe` [ ((Nothing, OpName "<+>"), NotRead (unreadOnly "Opaque")),
                     ((Just "O", OpName "<+>"), NotRead (unreadOnly "Opaque"))
                   ]

    it "leaves a settled qualified use out, unread imports notwithstanding" $
      unsettledIn "module M where\nimport qualified Data.Map as M\nimport Opaque\nf m = m M.! 1\n"
        `shouldBe` []

    it "holds an ambiguous operator against its unqualified use only" $
      unsettledIn
        "module M where\nimport Data.Map\nimport Other\nimport qualified Data.Map as M\nf a b = (a ! b, a M.! b)\n"
        `shouldBe` [((Nothing, OpName "!"), Ambiguous bangDisagreeing)]

    it "holds a clashing alias against the use written under it" $
      unsettledIn
        "module M where\nimport qualified Data.Map as M\nimport qualified Other as M\nf a b = a M.! b\n"
        `shouldBe` [((Just "M", OpName "!"), Ambiguous bangDisagreeing)]

    it "leaves the bare operator alone when only an alias is in doubt" $
      unsettledIn
        "module M where\nimport Data.Map\nimport qualified Data.Map as M\nimport qualified Other as M\nf a b = (a ! b, a M.! b)\n"
        `shouldBe` [((Just "M", OpName "!"), Ambiguous bangDisagreeing)]

    it "names a module imported twice once" $
      unsettledIn "module M where\nimport Data.Map\nimport Data.Map ((!))\nimport Other\nf a b = a ! b\n"
        `shouldBe` [((Nothing, OpName "!"), Ambiguous bangDisagreeing)]

    it "names every import that brings the operator in, agreeing or not" $
      unsettledIn "module M where\nimport Data.Map\nimport Other\nimport Agreeing\nf a b = a ! b\n"
        `shouldBe` [ ( (Nothing, OpName "!"),
                       Ambiguous (bangDisagreeing <> (("Agreeing", Fixity LeftAssoc 9) :| []))
                     )
                   ]

    it "spells a use the way the module wrote it" $
      fmap (uncurry operatorSpelling . fst) (unsettledIn "module M where\nimport qualified Opaque as O\nf a b = a O.<+> b\n")
        `shouldBe` ["O.<+>"]

  describe "a module's configurations" $ do
    it "see what its branches see written in one module" $
      scopeOverLeaves conditionalImports
        `shouldBe` fullScope (blankCpp conditionalImports)

    it "do not take the Prelude implicitly where one of them imports it" $
      fmap importModule (moduleImports (Is #implicitPrelude) (leavesOf conditionalImports))
        `shouldBe` ["Data.Map", "Data.Sequence", "Prelude", "Opaque"]

  describe "parsing with the module's own pragmas"
    $ it "parses a module that needs an extension it declares"
    $ declaredIn "{-# LANGUAGE MagicHash #-}\nmodule M where\ninfixl 6 <+>\n"
      `shouldBe` [(OpName "<+>", Fixity LeftAssoc 6)]

----------------------------------------------------------------------------
-- Helpers

-- | Stand-in for layer 3. The real one reads a build plan, maps modules to
-- packages and parses their sources; what it returns is exactly this shape,
-- so everything above it can be exercised without any of that.
-- | What @Data.Map@ and @Other@ each bring @!@ in with, in that order.
bangDisagreeing :: NonEmpty (Text, Fixity)
bangDisagreeing =
  ("Data.Map", Fixity LeftAssoc 9) :| [("Other", Fixity RightAssoc 4)]

-- | What @Terms@ and @Other.Terms@ each bring @:>@ in with, in that order.
termsDisagreeing :: NonEmpty (Text, Fixity)
termsDisagreeing =
  ("Terms", Fixity LeftAssoc 5) :| [("Other.Terms", Fixity RightAssoc 9)]

exportsOf :: Text -> Maybe Fixities
exportsOf = \case
  "Data.Map" -> whichever [(OpName "!", Fixity LeftAssoc 9)]
  "Data.Sequence" ->
    whichever
      [ (OpName "|>", Fixity LeftAssoc 5),
        (OpName "<|", Fixity RightAssoc 5)
      ]
  "Other" -> whichever [(OpName "!", Fixity RightAssoc 4)]
  "Agreeing" -> whichever [(OpName "!", Fixity LeftAssoc 9)]
  "Members" ->
    whichever
      [ (OpName ":|", Fixity RightAssoc 5),
        (OpName "<+>", Fixity LeftAssoc 6)
      ]
  -- Spell @:>@ in different namespaces, as servant and text do.
  "Types" -> Just (Map.fromList [((InTypes, OpName ":>"), Fixity RightAssoc 4)])
  "Terms" -> Just (Map.fromList [((InTerms, OpName ":>"), Fixity LeftAssoc 5)])
  "Other.Terms" -> Just (Map.fromList [((InTerms, OpName ":>"), Fixity RightAssoc 9)])
  "Opaque" -> Nothing
  "Other.Opaque" -> Nothing
  _ -> Just Map.empty
  where
    whichever = Just . inBothNamespaces . Map.fromList

parsed :: Text -> ParsedModule
parsed src = case parseModule defaultParserConfig "test.hs" src of
  Left _ -> error "the test input did not parse"
  Right pm -> pm

-- | The fixities a module declares, by name alone.
--
-- One entry per operator: a fixity that governs both namespaces is one
-- declaration, however many places it lands in.
declaredIn :: Text -> [(OpName, Fixity)]
declaredIn =
  Map.toList . Map.fromList . fmap (\((_, op), fixity) -> (op, fixity)) . declaredWithNamespaces

-- | The same, keeping the namespace each governs.
declaredWithNamespaces :: Text -> [((Namespace, OpName), Fixity)]
declaredWithNamespaces = Map.toList . declaredFixities . pmModule . parsed

exportsOfSource :: Text -> Maybe [ExportItem]
exportsOfSource = moduleExports . pmModule . parsed

-- | A scope knowing what every module it can read exports, and nothing
-- about the ones it cannot.
fullScope :: Text -> Scope
fullScope =
  resolveScope (Is #implicitPrelude) knowingExports . pure . pmModule . parsed

-- | A scope over a module's branch leaves, each parsed on its own.
scopeOverLeaves :: Text -> Scope
scopeOverLeaves =
  resolveScope (Is #implicitPrelude) knowingExports . leavesOf

-- | A module's branch leaves, parsed.
leavesOf :: Text -> NonEmpty (HsModule GhcPs)
leavesOf source = case nonEmpty =<< either (const Nothing) Just (branchLeaves source) of
  Nothing -> error "the test input has no branch leaves"
  Just texts -> fmap (pmModule . parsed) texts

-- | A module whose imports and fixity declarations are behind conditionals,
-- one of them importing the Prelude, and whose branches parse together.
conditionalImports :: Text
conditionalImports =
  T.unlines
    [ "module M where",
      "import Data.Map ((!))",
      "#if A",
      "import Data.Sequence",
      "import Prelude ()",
      "#else",
      "import Opaque",
      "#endif",
      "#if B",
      "infixr 5 +++",
      "#endif",
      "(+++) = (++)"
    ]

-- | What is known in a world made of 'exportsOf' alone.
knowingExports :: Text -> Established
knowingExports = maybe unreadable (\fixities -> mempty{establishedFixities = fixities}) . exportsOf

-- | The one import blamed for an operator, unread on its own account and so
-- with nothing below it. What every answer here was before a chain could be
-- reported at all.
unreadOnly :: Text -> NonEmpty ModuleChain
unreadOnly m = ModuleChain (m :| []) :| []

-- | A module that uses an operator nothing in scope declares, alongside an
-- import that could not be read.
usesUnknown :: Text
usesUnknown = "module M where\nimport Opaque\nf a b = a <??> b\n"

-- | A module that uses a function it defines itself infix, alongside an
-- import that could not be read.
definesItsOwn :: Text
definesItsOwn =
  "module M where\nimport Opaque\nw = 1 `clamp` 2\nclamp :: Int -> Int -> Int\nclamp = max\n"

-- | A Prelude that brings @min@ in, which declares no fixity for it.
preludeBringsMin :: [(Text, [(Namespace, Text)])]
preludeBringsMin = [("Prelude", [(InTerms, "min")])]

-- | A scope over a module with these imports, in a world where the modules
-- listed bring these names in, by namespace.
scopeBringing :: [(Text, [(Namespace, Text)])] -> Text -> Scope
scopeBringing said source =
  resolveScope
    (Is #implicitPrelude)
    (\m -> (knowingExports m){establishedCertain = certain m})
    (pure (pmModule (parsed ("module M where\n" <> source))))
  where
    certain m =
      Certain (Set.fromList [(namespace, OpName op) | (namespace, op) <- concat (lookup m said)]) Map.empty

-- | The same, with a second unread import to tell apart from the first.
twoUnread :: Text
twoUnread = "module M where\nimport Opaque\nimport Other.Opaque\nf a b = a <??> b\n"

-- | A scope in which the unread modules listed say which names they leave
-- unsettled.
--
-- A module absent from the list says nothing, which is what 'fullScope'
-- assumes of every one of them.
scopeKnowing :: [(Text, [Text])] -> Text -> Scope
scopeKnowing said =
  resolveScope
    (Is #implicitPrelude)
    (\m -> maybe (knowingExports m) leaving (lookup m said))
    . pure
    . pmModule
    . parsed
  where
    leaving ops =
      mempty
        { establishedUnsettled =
            Map.singleton [] (Set.fromList [(namespace, OpName op) | op <- ops, namespace <- [InTypes, InTerms]])
        }

-- | The uses of an operator a local binding captures in a module made of
-- these declarations, by where each operator starts.
capturedIn :: Text -> [((Int, Int), Fixity)]
capturedIn decls =
  [ ((spanStartLine s - 1, spanStartColumn s), fixity)
  | (s, fixity) <- Map.toList (capturedUses (pmGathered (parsed ("module M where\n" <> decls))))
  ]

-- | The members of each name a module declares, by namespace, in a
-- settled order.
declaredMembersIn :: Text -> [(OpName, [(Namespace, OpName)])]
declaredMembersIn = fmap (fmap Set.toList) . Map.toList . declaredMembers . pmModule . parsed

-- | The same, as the module's export list hands them on.
listedMembersIn :: Text -> [(OpName, [OpName])]
listedMembersIn = settled . listedMembers . pmModule . parsed

settled :: Map.Map OpName (Set.Set OpName) -> [(OpName, [OpName])]
settled = fmap (fmap Set.toList) . Map.toList

-- | A scope over a world where Members exports @T@ with the member @:|@.
--
-- Members declares @<+>@ as well, a member of nothing, so that a list naming
-- @T(..)@ can be seen to bring the one in and leave the other out.
scopeWithMembers :: Text -> Scope
scopeWithMembers source =
  resolveScope
    (Is #implicitPrelude)
    (\m -> (knowingExports m){establishedMembers = membersIn m})
    (pure (pmModule (parsed ("module M where\n" <> source))))
  where
    membersIn = \case
      "Members" -> Map.fromList [(OpName "T", Set.fromList [OpName ":|"])]
      _ -> Map.empty

-- | A scope over the two modules that spell @:>@ in different namespaces.
scopeOfBoth :: Text -> Scope
scopeOfBoth source =
  resolveScope
    (Is #implicitPrelude)
    knowingExports
    (pure (pmModule (parsed ("module M where\n" <> source))))

-- | A scope over an unread module, told the members of its names.
--
-- Opaque cannot be read for fixities and says nothing about what it
-- exports, so what the import list brings in is all there is to go on.
scopeSuspecting :: [(Text, [(Text, [Text])])] -> Text -> Scope
scopeSuspecting members source =
  resolveScope
    (Is #implicitPrelude)
    (\m -> (knowingExports m){establishedMembers = membersIn m})
    (pure (pmModule (parsed ("module M where\n" <> source <> "f a b = a <??> b\n"))))
  where
    membersIn m =
      Map.fromList
        [ (OpName parent, Set.fromList (fmap OpName kids))
        | (parent, kids) <- Map.findWithDefault [] m (Map.fromList members)
        ]

-- | The uses of an operator a module makes that its scope cannot settle.
unsettledIn :: Text -> [((Maybe Text, OpName), Unknown)]
unsettledIn src =
  let p = parsed src
   in unknownOperators
        (resolveScope (Is #implicitPrelude) knowingExports (pure (pmModule p)))
        (pure (pmGathered p))

-- | A module that takes its Prelude from elsewhere and hides an operator
-- from it, in order to take that operator from a module which spells it the
-- other way round.
takesItsPreludeElsewhere :: Text
takesItsPreludeElsewhere =
  "module M where\nimport Prelude.Compat hiding ((<%>))\nimport Pretty\n"

-- | The same, going on to use the operator it took.
usingItBothWays :: Text
usingItBothWays = takesItsPreludeElsewhere <> "f a b = a <%> b\n"

-- | A world in which the Prelude and a pretty-printer spell one operator
-- the two different ways, as @base@ and @pretty@ really do for @<>@.
--
-- Kept out of 'exportsOf' so that a Prelude which declares something does
-- not have to be reckoned with by every other test in the file.
disagreeingAboutPrelude :: Text -> Established
disagreeingAboutPrelude = maybe unreadable (\fixities -> mempty{establishedFixities = fixities}) . said
  where
    said = \case
      "Prelude" -> whichever [(OpName "<%>", Fixity RightAssoc 6)]
      "Pretty" -> whichever [(OpName "<%>", Fixity LeftAssoc 6)]
      _ -> Just Map.empty
    whichever = Just . inBothNamespaces . Map.fromList

-- | That world's scope for a module, told whether it has the Prelude.
scopeAboutPrelude :: Choice "implicitPrelude" -> Text -> Scope
scopeAboutPrelude implicitPrelude =
  resolveScope implicitPrelude disagreeingAboutPrelude . pure . pmModule . parsed

-- | What that world leaves unsettled in a module.
unsettledAboutPrelude ::
  Choice "implicitPrelude" ->
  Text ->
  [((Maybe Text, OpName), Unknown)]
unsettledAboutPrelude implicitPrelude src =
  let p = parsed src
   in unknownOperators
        (resolveScope implicitPrelude disagreeingAboutPrelude (pure (pmModule p)))
        (pure (pmGathered p))

scopeOf ::
  Text ->
  ( [(OpName, Fixity)],
    [((Text, OpName), Fixity)],
    [(Maybe Text, OpName)]
  )
scopeOf src =
  let reach = scopeInTerms (fullScope src)
   in ( Map.toList (Map.map fst (reachUnqualified reach)),
        Map.toList (Map.map fst (reachQualified reach)),
        Map.keys (reachAmbiguous reach)
      )
