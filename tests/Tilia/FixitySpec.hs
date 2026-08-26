{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Whether fixities can be resolved exactly from source alone.
module Tilia.FixitySpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Hspec
import Tilia.Fixity
import Tilia.Parser

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

  describe "layer 2: imports" $ do
    it "sees an unqualified import in both scopes" $
      -- A plain import brings names in qualified as well: @Data.Map.!@ is
      -- valid after @import Data.Map@.
      scopeOf "module M where\nimport Data.Map\n"
        `shouldBe`
          ( [(OpName "!", Fixity LeftAssoc 9)],
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
        `shouldBe`
          ( [(OpName "!", Fixity LeftAssoc 9)],
            [(("M", OpName "!"), Fixity LeftAssoc 9)],
            []
          )

    it "honours an explicit import list" $
      scopeOf "module M where\nimport Data.Sequence ((|>))\n"
        `shouldBe`
          ( [(OpName "|>", Fixity LeftAssoc 5)],
            [(("Data.Sequence", OpName "|>"), Fixity LeftAssoc 5)],
            []
          )

    it "honours a hiding list" $
      scopeOf "module M where\nimport Data.Sequence hiding ((|>))\n"
        `shouldBe`
          ( [(OpName "<|", Fixity RightAssoc 5)],
            [(("Data.Sequence", OpName "<|"), Fixity RightAssoc 5)],
            []
          )

    it "lets the module's own declaration win over an import" $
      let (unq, _, _) = scopeOf "module M where\nimport Data.Map\ninfixr 3 !\n"
       in unq `shouldBe` [(OpName "!", Fixity RightAssoc 3)]

  describe "ambiguity" $ do
    it "reports an operator imported with two different fixities" $
      let (_, _, amb) = scopeOf "module M where\nimport Data.Map\nimport Other\n"
       in amb `shouldBe` [OpName "!"]

    it "reports nothing when two imports agree" $
      let (_, _, amb) = scopeOf "module M where\nimport Data.Map\nimport Agreeing\n"
       in amb `shouldBe` []

    it "reports nothing when the clash is only in qualified scope" $
      let (_, _, amb) =
            scopeOf "module M where\nimport Data.Map\nimport qualified Other\n"
       in amb `shouldBe` []

  describe "lookupFixity" $ do
    it "finds an unqualified operator" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s Nothing (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "finds a qualified operator through its alias" $
      let s = fullScope "module M where\nimport qualified Data.Map as M\n"
       in lookupFixity s (Just "M") (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "concludes infixl 9 when every module in scope was read" $
      let s = fullScope "module M where\nimport Data.Map\n"
       in lookupFixity s Nothing (OpName "<??>")
            `shouldBe` Resolved defaultFixity ReportDefault

    it "refuses to conclude anything when a module could not be read" $
      let s = fullScope "module M where\nimport Data.Map\nimport Opaque\n"
       in lookupFixity s Nothing (OpName "<??>")
            `shouldBe` Unresolved ["Opaque"]

    it "still answers for an operator it did find, despite an unread module" $
      let s = fullScope "module M where\nimport Data.Map\nimport Opaque\n"
       in lookupFixity s Nothing (OpName "!")
            `shouldBe` Resolved (Fixity LeftAssoc 9) (DeclaredIn "Data.Map")

    it "attributes the module\'s own declaration to itself" $
      let s = fullScope "module M where\ninfixr 3 <+>\n"
       in lookupFixity s Nothing (OpName "<+>")
            `shouldBe` Resolved (Fixity RightAssoc 3) DeclaredHere

    it "does not find a qualified-only operator unqualified" $
      let s = fullScope "module M where\nimport qualified Data.Map\n"
       in lookupFixity s Nothing (OpName "!")
            `shouldBe` Resolved defaultFixity ReportDefault

  describe "parsing with the module's own pragmas" $
    it "parses a module that needs an extension it declares" $
      -- Without reading the pragma this does not parse at all.
      declaredIn "{-# LANGUAGE MagicHash #-}\nmodule M where\ninfixl 6 <+>\n"
        `shouldBe` [(OpName "<+>", Fixity LeftAssoc 6)]

----------------------------------------------------------------------------
-- Helpers

-- | Stand-in for layer 3. The real one reads a build plan, maps modules to
-- packages and parses their sources; what it returns is exactly this shape,
-- so everything above it can be exercised without any of that.
exportsOf :: Text -> Maybe (Map.Map OpName Fixity)
exportsOf = \case
  "Data.Map" -> Just (Map.fromList [(OpName "!", Fixity LeftAssoc 9)])
  "Data.Sequence" ->
    Just
      ( Map.fromList
          [ (OpName "|>", Fixity LeftAssoc 5),
            (OpName "<|", Fixity RightAssoc 5)
          ]
      )
  -- Declares @!@ differently from Data.Map, so importing both unqualified
  -- is ambiguous.
  "Other" -> Just (Map.fromList [(OpName "!", Fixity RightAssoc 4)])
  -- Declares @!@ the same way, as a re-export would.
  "Agreeing" -> Just (Map.fromList [(OpName "!", Fixity LeftAssoc 9)])
  -- Stands for a module we could not read at all.
  "Opaque" -> Nothing
  _ -> Just Map.empty

parsed :: Text -> ParsedModule
parsed src = case parseModule defaultParserConfig "test.hs" src of
  Left _ -> error "the test input did not parse"
  Right pm -> pm

declaredIn :: Text -> [(OpName, Fixity)]
declaredIn = Map.toList . declaredFixities . pmModule . parsed

fullScope :: Text -> Scope
fullScope = resolveScope exportsOf . pmModule . parsed

scopeOf :: Text -> ([(OpName, Fixity)], [((Text, OpName), Fixity)], [OpName])
scopeOf src =
  let s = fullScope src
   in ( Map.toList (Map.map fst (scopeUnqualified s)),
        Map.toList (Map.map fst (scopeQualified s)),
        scopeAmbiguous s
      )
