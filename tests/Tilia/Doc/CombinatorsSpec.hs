{-# LANGUAGE OverloadedStrings #-}

-- | The vocabulary printing code is written in.
module Tilia.Doc.CombinatorsSpec (spec) where

import Test.Hspec

import Tilia.Doc
import Tilia.Doc.Combinators
import Tilia.Span
import Tilia.TestUtils (withEachSettings)

spec :: Spec
spec = withEachSettings $ \settings -> do
  let out = printDoc settings
  describe "groups" $ do
    it "becomes a space when flat" $
      out (flat (txt "a" <> breakOrSpace <> txt "b")) `shouldBe` "a b\n"
    it "becomes a break when broken" $
      out (broken (txt "a" <> breakOrSpace <> txt "b")) `shouldBe` "a\nb\n"
    it "leaves nothing when flat" $
      out (flat (txt "a" <> breakOrNothing <> txt "b")) `shouldBe` "ab\n"
    it "becomes a break when broken, leaving nothing behind" $
      out (broken (txt "a" <> breakOrNothing <> txt "b")) `shouldBe` "a\nb\n"
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

  describe "provenance" $
    it "does not affect layout" $ do
      let d = txt "a" <> breakOrSpace <> txt "b"
          s = mkSpan (1, 1) (1, 9)
      out (flat (located s d)) `shouldBe` out (flat d)

  describe "combining" $ do
    it "separates with commaSep when flat" $ do
      out (flat (commaSep TrailingCommas [txt "a", txt "b", txt "c"]))
        `shouldBe` "a, b, c\n"
      out (flat (commaSep LeadingCommas [txt "a", txt "b", txt "c"]))
        `shouldBe` "a, b, c\n"
    it "keeps trailing commas on the line above when broken" $
      out (broken (commaSep TrailingCommas [txt "a", txt "b"]))
        `shouldBe` "a,\nb\n"
    it "starts the line below with a leading comma when broken" $
      out (broken (commaSep LeadingCommas [txt "a", txt "b"]))
        `shouldBe` "a\n, b\n"
    it "handles an empty list" $
      out (flat (commaSep TrailingCommas [])) `shouldBe` ""
    it "handles a single element" $
      out (broken (commaSep LeadingCommas [txt "a"])) `shouldBe` "a\n"
    it "puts a separating comma before what follows" $
      out (broken (txt "a" <> separatingComma LeadingCommas <> txt "b"))
        `shouldBe` "a\n, b\n"
    it "lines items up after a lead-in" $ do
      let items = [txt "a", txt "b"]
      out (broken (commaSepAfter TrailingCommas "|" items))
        `shouldBe` "| a,\n  b\n"
      out (broken (commaSepAfter LeadingCommas "|" items))
        `shouldBe` "| a\n, b\n"
    it "joins with hsep" $
      out (flat (hsep [txt "a", txt "b"])) `shouldBe` "a b\n"
    it "joins with vsep" $
      out (flat (vsep [txt "a", txt "b"])) `shouldBe` "a\nb\n"

  describe "brackets" $ do
    it "adds nothing when flat" $
      out (flat (parens (commaSep TrailingCommas [txt "a", txt "b"])))
        `shouldBe` "(a, b)\n"
    it "keeps the opening bracket company when broken" $
      out (broken (parens (commaSep TrailingCommas [txt "a", txt "b"])))
        `shouldBe` "( a,\n  b\n)\n"
    it "lines the body up under itself" $ do
      let items = [txt "a", txt "b", txt "c"]
      out (broken (brackets (commaSep TrailingCommas items)))
        `shouldBe` "[ a,\n  b,\n  c\n]\n"
    it "keeps the closing bracket in when asked" $ do
      let items = [txt "a", txt "b"]
      out (broken (parensWith Indented (commaSep TrailingCommas items)))
        `shouldBe` "( a,\n  b\n  )\n"
    it "puts leading commas under the opening bracket" $
      out (broken (leading Outdented [txt "a", txt "b", txt "c"]))
        `shouldBe` "[ a\n, b\n, c\n]\n"
    it "puts leading commas a step in with the closing bracket" $
      out (broken (leading Indented [txt "a", txt "b"]))
        `shouldBe` "[ a\n  , b\n  ]\n"
    it "adds nothing to leading commas when flat" $
      out (flat (leading Outdented [txt "a", txt "b"])) `shouldBe` "[a, b]\n"
    it "renders an empty bracket pair flat" $
      out (flat (brackets mempty)) `shouldBe` "[]\n"
    it "spaces the unboxed pair" $
      out (flat (unboxed (commaSep TrailingCommas [txt "a", txt "b"])))
        `shouldBe` "(# a, b #)\n"
    it "gives a spaced pair its own lines when broken" $
      out (broken (unboxed (commaSep TrailingCommas [txt "a", txt "b"])))
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

-- | Items in square brackets, with leading commas.
leading :: ClosingIndent -> [Doc] -> Doc
leading closing =
  commaBracketWith LeadingCommas closing "[" "]" . commaSep LeadingCommas
