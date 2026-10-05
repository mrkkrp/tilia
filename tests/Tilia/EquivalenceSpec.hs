{-# LANGUAGE OverloadedStrings #-}

-- | Whether formatting changed what a module says.
module Tilia.EquivalenceSpec (spec) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import Test.Hspec
import Tilia.Equivalence
import Tilia.Parser
  ( ParsedModule,
    defaultParserConfig,
    describeParseError,
    parseModule,
    pmModule,
  )

spec :: Spec
spec = do
  describe "what a formatter is allowed to do" $ do
    it "sees past layout" $
      "module M where\nf x   =    x\n" `saysTheSameAs` "module M where\n\nf x = x\n"

    it "sees past a line broken in a different place" $
      "module M where\nf x = x + 1\n"
        `saysTheSameAs` "module M where\nf x =\n  x\n    + 1\n"

    it "sees past the two spellings of a qualified import" $
      "module M where\nimport qualified Data.Map as M\n"
        `saysTheSameAs` "module M where\nimport Data.Map qualified as M\n"

    it "sees past the order the imports were written in" $
      "module M where\nimport Data.Set\nimport Data.Map\n"
        `saysTheSameAs` "module M where\nimport Data.Map\nimport Data.Set\n"

    it "sees past the brackets around a deriving clause" $
      "module M where\ndata T = T deriving Eq\n"
        `saysTheSameAs` "module M where\ndata T = T deriving (Eq)\n"

    it "sees past an empty context" $
      "module M where\nclass () => C a where\n  m :: a\n"
        `saysTheSameAs` "module M where\nclass C a where\n  m :: a\n"

    it "sees past the brackets around one constraint" $
      "module M where\nf :: (Show a) => a -> a\nf x = x\n"
        `saysTheSameAs` "module M where\nf :: Show a => a -> a\nf x = x\n"

    it "sees past a documentation comment set differently" $
      "module M where\n\n-- | Says something.\nf :: Int\nf = 1\n"
        `saysTheSameAs` "module M where\n\n-- |    Says   something.\nf :: Int\nf = 1\n"

    it "sees past a documentation comment that says nothing at all" $
      "module M where\n\n-- |\nf :: Int\nf = 1\n"
        `saysTheSameAs` "module M where\n\nf :: Int\nf = 1\n"

    it "sees past a documented argument whose documentation says nothing" $
      "module M where\n\nf ::\n  Int ->\n  -- ^ \n  Int\nf = id\n"
        `saysTheSameAs` "module M where\n\nf ::\n  Int ->\n  -- ^\n  Int\nf = id\n"

    it "sees past comments, which are not the tree's business" $
      "module M where\n\n-- a remark\nf :: Int\nf = 1\n"
        `saysTheSameAs` "module M where\n\nf :: Int\nf = 1\n"

  describe "what it must not let through" $ do
    it "catches a changed literal" $
      "module M where\nf = 1\n" `saysSomethingElseThan` "module M where\nf = 2\n"

    it "catches a changed name" $
      "module M where\nf x = x\n" `saysSomethingElseThan` "module M where\nf x = y\n"

    it "catches an operator regrouped" $
      "module M where\nf = a + b * c\n"
        `saysSomethingElseThan` "module M where\nf = (a + b) * c\n"

    it "catches a declaration dropped" $
      "module M where\nf = 1\ng = 2\n" `saysSomethingElseThan` "module M where\nf = 1\n"

    it "catches an import dropped" $
      "module M where\nimport Data.Map\nimport Data.Set\n"
        `saysSomethingElseThan` "module M where\nimport Data.Map\n"

    it "catches an import list losing an entry" $
      "module M where\nimport Data.List (sort, nub)\n"
        `saysSomethingElseThan` "module M where\nimport Data.List (sort)\n"

    it "catches an import that stopped being qualified" $
      "module M where\nimport qualified Data.Map as M\n"
        `saysSomethingElseThan` "module M where\nimport Data.Map as M\n"

    it "catches an export list losing an entry" $
      "module M (f, g) where\nf = 1\ng = 2\n"
        `saysSomethingElseThan` "module M (f) where\nf = 1\ng = 2\n"

    it "catches a documentation comment losing a word" $
      "module M where\n\n-- | Says something.\nf :: Int\nf = 1\n"
        `saysSomethingElseThan` "module M where\n\n-- | Says.\nf :: Int\nf = 1\n"

    it "says where the difference is, not merely that there is one" $
      case syntaxDifference (treeOf "module M where\nf = 1\n") (treeOf "module M where\nf = 2\n") of
        Nothing -> expectationFailure "found no difference"
        Just why -> do
          why `shouldSatisfy` T.isInfixOf "HsOverLit"
          why `shouldSatisfy` T.isInfixOf "1 became 2"

  describe "the comments a formatter must carry over" $ do
    it "is content when they all came through" $
      "module M where\n\n-- a remark\nf = 1\n"
        `keepsTheCommentsOf` "module M where\n\n-- a remark\nf = 1\n"

    it "notices one that went missing" $
      "module M where\n\n-- a remark\nf = 1\n"
        `losesTheCommentsOf` "module M where\n\nf = 1\n"

    it "notices one that was invented" $
      "module M where\n\nf = 1\n"
        `losesTheCommentsOf` "module M where\n\n-- a remark\nf = 1\n"

    it "notices a pragma that went missing, and names it" $
      case difference "{-# LANGUAGE LambdaCase #-}\nmodule M where\nf = 1\n" "module M where\nf = 1\n" of
        Nothing -> expectationFailure "let the pragma go"
        Just why -> do
          why `shouldSatisfy` T.isInfixOf "lost the pragma"
          why `shouldSatisfy` T.isInfixOf "LambdaCase"

    it "notices a pragma that was invented" $
      case difference "module M where\nf = 1\n" "{-# LANGUAGE LambdaCase #-}\nmodule M where\nf = 1\n" of
        Nothing -> expectationFailure "let the pragma through"
        Just why -> why `shouldSatisfy` T.isInfixOf "invented the pragma"

    it "does not mind the order of what stands above the module header" $
      "{-# LANGUAGE LambdaCase #-}\n{-# LANGUAGE TupleSections #-}\nmodule M where\nf = 1\n"
        `keepsTheCommentsOf` "{-# LANGUAGE TupleSections #-}\n{-# LANGUAGE LambdaCase #-}\nmodule M where\nf = 1\n"

    it "does not mind a comment that moved with the import it belongs to" $
      "module M where\n\n-- about Set\nimport Data.Set\n\n-- about Map\nimport Data.Map\n"
        `keepsTheCommentsOf` "module M where\n\n-- about Map\nimport Data.Map\n\n-- about Set\nimport Data.Set\n"

----------------------------------------------------------------------------
-- Helpers

-- | Two modules that a formatter could turn one into the other.
saysTheSameAs :: Text -> Text -> Expectation
saysTheSameAs went came =
  syntaxDifference (treeOf went) (treeOf came) `shouldBe` Nothing

-- | Two modules that no formatter may turn one into the other.
saysSomethingElseThan :: Text -> Text -> Expectation
saysSomethingElseThan went came =
  syntaxDifference (treeOf went) (treeOf came) `shouldSatisfy` isJust

keepsTheCommentsOf :: Text -> Text -> Expectation
keepsTheCommentsOf went came = difference went came `shouldBe` Nothing

losesTheCommentsOf :: Text -> Text -> Expectation
losesTheCommentsOf went came =
  difference went came `shouldSatisfy` isJust

-- | What 'commentDifference' makes of two spellings of a module.
difference :: Text -> Text -> Maybe Text
difference went came = commentDifference (parsed went) (parsed came)

treeOf :: Text -> HsModule GhcPs
treeOf = pmModule . parsed

parsed :: Text -> ParsedModule
parsed source = case parseModule defaultParserConfig "Test.hs" source of
  Left problem -> error (T.unpack (describeParseError problem))
  Right m -> m
