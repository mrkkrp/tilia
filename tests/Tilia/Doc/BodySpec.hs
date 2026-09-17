{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Placement, attachment, and the 'Body' class.
module Tilia.Doc.BodySpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Tilia.Doc
import Tilia.Doc.Body
import Tilia.Doc.Combinators

-- | A stand-in for a real body type, enough to exercise the class: one
-- construct that hangs, one that does not, and one that takes its answer
-- from another.
data Toy
  = Call Text Text
  | Block [Text]
  | Apply Toy Toy

instance Body Toy where
  printBody = \case
    Call f x -> txt f <> space <> txt x
    Block ss -> txt "do" <> indent (hardBreak <> sepBy hardBreak (fmap txt ss))
    Apply f x -> printBody f <> space <> printBody x

  bodyPlacement = \case
    Call _ _ -> Normal
    Block _ -> Hanging
    Apply _ x -> bodyPlacement x

spec :: Spec
spec = do
  describe "attach" $ do
    it "hands over the line when hanging" $
      out (broken (txt "=" <> attach Hanging (txt "do" <> indent (hardBreak <> txt "s"))))
        `shouldBe` "= do\n  s\n"
    it "breaks and indents when normal" $
      out (broken (txt "=" <> attach Normal (txt "f" <> space <> txt "x")))
        `shouldBe` "=\n  f x\n"
    it "stays on one line when flat, either way" $ do
      out (flat (txt "=" <> attach Normal (txt "x"))) `shouldBe` "= x\n"
      out (flat (txt "=" <> attach Hanging (txt "x"))) `shouldBe` "= x\n"

  describe "Body" $ do
    it "attaches a hanging body" $
      out (broken (txt "=" <> attachBody (Block ["a", "b"])))
        `shouldBe` "= do\n  a\n  b\n"
    it "attaches a normal body" $
      out (broken (txt "=" <> attachBody (Call "f" "x")))
        `shouldBe` "=\n  f x\n"
    it "propagates placement through an application" $
      bodyPlacement (Apply (Call "f" "x") (Block ["a"])) `shouldBe` Hanging
    it "stops propagating at a non-hanging tail" $
      bodyPlacement (Apply (Block ["a"]) (Call "f" "x")) `shouldBe` Normal
    it "propagates through nesting" $
      bodyPlacement (Apply (Call "f" "x") (Apply (Call "g" "y") (Block ["a"])))
        `shouldBe` Hanging

out :: Doc -> Text
out = printDoc defaultRenderOptions
