{-# LANGUAGE LambdaCase #-}

-- | Regrouping operator chains by precedence.
module Tilia.Render.OperatorSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Tilia.Fixity (Direction (..), Fixity (..))
import Tilia.Render.Operator

spec :: Spec
spec = do
  describe "flatten" $ do
    it "leaves a leaf alone" $
      flatten split (Leaf 'a') `shouldBe` (Leaf 'a' :| [], [])
    it "reads a chain left to right" $
      flatten split (Apply (Apply (Leaf 'a') '+' (Leaf 'b')) '*' (Leaf 'c'))
        `shouldBe` (Leaf 'a' :| [Leaf 'b', Leaf 'c'], ['+', '*'])
    it "does not go inside a leaf" $
      flatten split (Apply (Leaf 'a') '+' (Opaque (Apply (Leaf 'b') '*' (Leaf 'c'))))
        `shouldBe` (Leaf 'a' :| [Opaque (Apply (Leaf 'b') '*' (Leaf 'c'))], ['+'])

  describe "associate" $ do
    it "makes one level of a chain that binds equally" $
      associate known (leaves "abc") ['+', '+']
        `shouldBe` Chain (Operand (Leaf 'a') :| [Operand (Leaf 'b'), Operand (Leaf 'c')]) ['+', '+']

    it "splits at the loosest operator" $
      associate known (leaves "abc") ['*', '+']
        `shouldBe` Chain
          ( Chain (Operand (Leaf 'a') :| [Operand (Leaf 'b')]) ['*']
              :| [Operand (Leaf 'c')]
          )
          ['+']

    it "puts every level in its place" $
      associate known (leaves "abcd") ['*', '+', '*']
        `shouldBe` Chain
          ( Chain (Operand (Leaf 'a') :| [Operand (Leaf 'b')]) ['*']
              :| [Chain (Operand (Leaf 'c') :| [Operand (Leaf 'd')]) ['*']]
          )
          ['+']

    it "keeps a single operand as one" $
      associate known (Leaf 'a' :| []) []
        `shouldBe` Operand (Leaf 'a')

    -- What the author wrote is the only information left when a fixity
    -- cannot be established, so it is what the layout follows.
    it "leaves the chain flat when one fixity is unknown" $
      associate known (leaves "abc") ['*', '?']
        `shouldBe` Chain (Operand (Leaf 'a') :| [Operand (Leaf 'b'), Operand (Leaf 'c')]) ['*', '?']

  describe "separators" $ do
    it "recognises an operator that introduces its operand" $
      isSeparator (Just (Fixity RightAssoc 0)) `shouldBe` True
    it "does not mistake a tight right-associative operator for one" $
      isSeparator (Just (Fixity RightAssoc 6)) `shouldBe` False
    it "says nothing about an operator it does not know" $
      isSeparator Nothing `shouldBe` False

----------------------------------------------------------------------------
-- A stand-in for the syntax tree

-- | Just enough of an expression to have operators in it.
data E
  = Leaf Char
  | Apply E Char E
  | -- | Something the chain builder must not look inside, standing in for a
    -- parenthesised subexpression.
    Opaque E
  deriving (Eq, Show)

split :: E -> Maybe (E, Char, E)
split = \case
  Apply l o r -> Just (l, o, r)
  _ -> Nothing

leaves :: [Char] -> NonEmpty E
leaves = \case
  [] -> error "leaves: none"
  (c : cs) -> Leaf c :| fmap Leaf cs

-- | @?@ is the operator nothing is known about.
known :: Char -> Maybe Fixity
known = \case
  '+' -> Just (Fixity LeftAssoc 6)
  '*' -> Just (Fixity LeftAssoc 7)
  _ -> Nothing
