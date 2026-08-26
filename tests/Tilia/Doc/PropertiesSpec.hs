{-# LANGUAGE OverloadedStrings #-}

-- | Properties that should hold of every document the engine renders.
module Tilia.Doc.PropertiesSpec (spec) where

import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Test.QuickCheck
import Tilia.Gen
import Tilia.Doc
import Tilia.Doc.Combinators
import Tilia.Span

spec :: Spec
spec = do
  describe "output shape" $ do
    it "never leaves trailing whitespace on a line" $
      property $ \(AnyDoc d) ->
        let ls = T.lines (out d)
         in counterexample (show ls) (all (\l -> l == T.stripEnd l) ls)

    it "is empty or ends in exactly one newline" $
      property $ \(AnyDoc d) ->
        let t = out d
         in T.null t || (T.isSuffixOf "\n" t && not (T.isSuffixOf "\n\n" t))

    it "never begins with a blank line" $
      property $ \(AnyDoc d) ->
        let t = out d
         in not (T.isPrefixOf "\n" t)

    it "never carries two blank lines in a row" $
      property $ \(AnyDoc d) ->
        let ls = T.lines (out d)
            pairs = zip ls (drop 1 ls)
         in counterexample (show ls) (not (any (\(a, b) -> T.null a && T.null b) pairs))

  describe "content" $
    it "emits exactly the text it was given, and nothing else" $
      property $ \(PlainDoc d) ->
        let expected = squash (T.concat (docTexts d))
            actual = squash (out d)
         in counterexample (show (expected, actual)) (expected == actual)

  describe "layout" $ do
    it "keeps a flat document on one line" $
      property $ \(FlatSafeDoc d) ->
        let t = out (flat d)
         in counterexample (show t) (length (T.lines t) <= 1)

    it "renders a group with a single-line span as flat" $
      property $ \(FlatSafeDoc d) (SingleLineSpan s) ->
        out (group s d) === out (flat d)

    it "renders a group with a multi-line span as broken" $
      property $ \(FlatSafeDoc d) (MultiLineSpan s) ->
        out (group s d) === out (broken d)

  describe "provenance" $
    it "does not affect the output" $
      property $ \(AnyDoc d) (AnySpan s) ->
        out (located s d) === out d

  describe "monoid" $ do
    it "renders associatively" $
      property $ \(AnyDoc a) (AnyDoc b) (AnyDoc c) ->
        out (broken ((a <> b) <> c)) === out (broken (a <> (b <> c)))

    it "has mempty as a rendering identity" $
      property $ \(AnyDoc d) -> do
        out (broken (mempty <> d)) === out (broken d)
          .&&. out (broken (d <> mempty)) === out (broken d)

  describe "Span" $ do
    it "unions associatively" $
      property $ \(AnySpan a) (AnySpan b) (AnySpan c) ->
        (a <> b) <> c === a <> (b <> c)

    it "unions to something covering both operands" $
      property $ \(AnySpan a) (AnySpan b) ->
        let u = a <> b
         in counterexample (show u) (covers u a && covers u b)

    it "is idempotent under union" $
      property $ \(AnySpan a) -> a <> a === a

-- | Everything that is not whitespace, in order.
--
-- Whitespace is exactly what the engine is entitled to add, move and
-- remove; what remains is what it must not touch.
squash :: Text -> Text
squash = T.filter (not . isSpace)

out :: Doc -> Text
out = printDoc defaultRenderOptions
