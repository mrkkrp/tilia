-- | The span algebra.
module Tilia.SpanSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Tilia.Gen (AnySpan (..))
import Tilia.Span

spec :: Spec
spec = do
  it "recognises a single-line span" $
    isSingleLine (mkSpan (3, 1) (3, 40)) `shouldBe` True
  it "recognises a multi-line span" $
    isSingleLine (mkSpan (3, 1) (4, 1)) `shouldBe` False
  it "unions to cover both operands" $
    mkSpan (1, 5) (1, 9) <> mkSpan (3, 2) (4, 1)
      `shouldBe` mkSpan (1, 5) (4, 1)
  it "takes the earlier start column when starts share a line" $
    mkSpan (1, 9) (1, 20) <> mkSpan (1, 3) (1, 5)
      `shouldBe` mkSpan (1, 3) (1, 20)

  it "unions associatively" $
    property $ \(AnySpan a) (AnySpan b) (AnySpan c) ->
      (a <> b) <> c === a <> (b <> c)

  it "unions to something covering both operands" $
    property $ \(AnySpan a) (AnySpan b) ->
      let u = a <> b
       in counterexample (show u) (covers u a && covers u b)

  it "is idempotent under union" $
    property $
      \(AnySpan a) -> a <> a === a
