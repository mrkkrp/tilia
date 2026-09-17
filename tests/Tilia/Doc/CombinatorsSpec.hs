{-# LANGUAGE OverloadedStrings #-}

-- | The vocabulary printing code is written in.
module Tilia.Doc.CombinatorsSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Tilia.Doc
import Tilia.Doc.Combinators
import Tilia.Span

spec :: Spec
spec = do
  describe "groups" $ do
    it "becomes a space when flat" $
      out (flat (txt "a" <> breakOrSpace <> txt "b")) `shouldBe` "a b\n"
    it "becomes a break when broken" $
      out (broken (txt "a" <> breakOrSpace <> txt "b")) `shouldBe` "a\nb\n"
    it "leaves nothing when flat" $
      out (flat (txt "a" <> breakOrNothing <> txt "b")) `shouldBe` "ab\n"
    it "becomes a break when broken, leaving nothing behind" $
      out (broken (txt "a" <> breakOrNothing <> txt "b")) `shouldBe` "a\nb\n"
    it "follows a single-line span" $
      out (group (mkSpan (1, 1) (1, 9)) (txt "a" <> breakOrSpace <> txt "b"))
        `shouldBe` "a b\n"
    it "follows a multi-line span" $
      out (group (mkSpan (1, 1) (2, 9)) (txt "a" <> breakOrSpace <> txt "b"))
        `shouldBe` "a\nb\n"
    it "lets an inner group override the enclosing layout" $
      out (flat (txt "a" <> broken (breakOrSpace <> txt "b")))
        `shouldBe` "a\nb\n"
    it "ignores a hard line's enclosing layout" $
      out (flat (txt "a" <> hardBreak <> txt "b")) `shouldBe` "a\nb\n"

  describe "variant" $ do
    it "takes the first branch when flat" $
      out (flat (variant (txt "one") (txt "many"))) `shouldBe` "one\n"
    it "takes the second branch when broken" $
      out (broken (variant (txt "one") (txt "many"))) `shouldBe` "many\n"
    it "follows the span like any other group" $ do
      let v = variant (txt "one") (txt "many")
      out (group (mkSpan (1, 1) (1, 9)) v) `shouldBe` "one\n"
      out (group (mkSpan (1, 1) (2, 9)) v) `shouldBe` "many\n"

  describe "provenance" $
    it "does not affect layout" $ do
      let d = txt "a" <> breakOrSpace <> txt "b"
          s = mkSpan (1, 1) (1, 9)
      out (flat (located s d)) `shouldBe` out (flat d)

  describe "combining" $ do
    it "separates with commaSep when flat" $
      out (flat (commaSep [txt "a", txt "b", txt "c"]))
        `shouldBe` "a, b, c\n"
    it "keeps commas on the line above when broken" $
      out (broken (commaSep [txt "a", txt "b"]))
        `shouldBe` "a,\nb\n"
    it "handles an empty list" $
      out (flat (commaSep [])) `shouldBe` ""
    it "handles a single element" $
      out (broken (commaSep [txt "a"])) `shouldBe` "a\n"
    it "joins with hsep" $
      out (flat (hsep [txt "a", txt "b"])) `shouldBe` "a b\n"
    it "joins with vsep" $
      out (flat (vsep [txt "a", txt "b"])) `shouldBe` "a\nb\n"

  describe "brackets" $ do
    it "adds nothing when flat" $
      out (flat (parens (commaSep [txt "a", txt "b"])))
        `shouldBe` "(a, b)\n"
    it "keeps the opening bracket company when broken" $
      out (broken (parens (commaSep [txt "a", txt "b"])))
        `shouldBe` "( a,\n  b\n)\n"
    it "lines the body up under itself" $
      out (broken (brackets (commaSep [txt "a", txt "b", txt "c"])))
        `shouldBe` "[ a,\n  b,\n  c\n]\n"
    it "keeps the closing bracket in when asked" $
      out (broken (parensWith Indented (commaSep [txt "a", txt "b"])))
        `shouldBe` "( a,\n  b\n  )\n"
    it "renders an empty bracket pair flat" $
      out (flat (brackets mempty)) `shouldBe` "[]\n"
    it "spaces the unboxed pair" $
      out (flat (unboxed (commaSep [txt "a", txt "b"])))
        `shouldBe` "(# a, b #)\n"
    it "gives a spaced pair its own lines when broken" $
      out (broken (unboxed (commaSep [txt "a", txt "b"])))
        `shouldBe` "(#\n  a,\n  b\n#)\n"
    it "wraps in backticks" $
      out (flat (backticks (txt "div"))) `shouldBe` "`div`\n"

  describe "conditionals" $ do
    it "includes when the condition holds" $
      out (flat (txt "a" <> includeWhen True (space <> txt "b")))
        `shouldBe` "a b\n"
    it "omits when it does not" $
      out (flat (txt "a" <> includeWhen False (space <> txt "b")))
        `shouldBe` "a\n"
    it "inverts with includeUnless" $
      out (flat (includeUnless True (txt "a"))) `shouldBe` ""

out :: Doc -> Text
out = printDoc defaultRenderOptions
