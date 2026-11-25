{-# LANGUAGE OverloadedStrings #-}

-- | The vocabulary printing code is written in.
module Tilia.Doc.CombinatorsSpec (spec) where

import Test.Hspec

import Tilia.Doc
import Tilia.Doc.Combinators
import Tilia.Span
import Tilia.TestUtils (indentedBy, withEachSettings)

spec :: Spec
spec = withEachSettings $ \settings -> do
  let out = printDoc settings
      step = indentedBy settings
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
      out
        ( broken
            ( txt "  "
                <> align (txt "a" <> separatingComma LeadingCommas <> txt "b")
            )
        )
        `shouldBe` "  a\n, b\n"
    it "lines items up after a lead-in" $ do
      let items = [txt "a", txt "b"]
      out (broken (commaSepAfter TrailingCommas "|" items))
        `shouldBe` "| a,\n  b\n"
      out (broken (commaSepAfter LeadingCommas "|" items))
        `shouldBe` "| a\n, b\n"
    it "joins with a separator" $
      out (flat (sepBy hardBreak [txt "a", txt "b"])) `shouldBe` "a\nb\n"

  describe "brackets" $ do
    it "adds nothing when flat" $
      out (flat (parens (commaSep TrailingCommas [txt "a", txt "b"])))
        `shouldBe` "(a, b)\n"
    it "keeps the opening bracket company when broken" $
      out (broken (parens (commaSep TrailingCommas [txt "a", txt "b"])))
        `shouldBe` "( a,\n  b\n)\n"
    it "lines the body up under itself" $ do
      let items = [txt "a", txt "b", txt "c"]
      out (broken (bracketWith 0 "[" "]" (commaSep TrailingCommas items)))
        `shouldBe` "[ a,\n  b,\n  c\n]\n"
    it "keeps the closing bracket in when asked" $ do
      let items = [txt "a", txt "b"]
      out (broken (bracketWith 1 "(" ")" (commaSep TrailingCommas items)))
        `shouldBe` "( a,\n  b\n" <> step 1 <> ")\n"
    it "puts leading commas under the opening bracket" $
      out (broken (ledByCommas 0 [txt "a", txt "b", txt "c"]))
        `shouldBe` "[ a\n, b\n, c\n]\n"
    it "puts leading commas under the first item where the closing bracket goes a step in" $
      out (broken (ledByCommas 1 [txt "a", txt "b"]))
        `shouldBe` "[ a\n  , b\n" <> step 1 <> "]\n"
    it "adds nothing to leading commas when flat" $
      out (flat (ledByCommas 0 [txt "a", txt "b"]))
        `shouldBe` "[a, b]\n"
    it "renders an empty bracket pair flat" $
      out (flat (bracketWith 0 "[" "]" mempty)) `shouldBe` "[]\n"
    it "spaces the unboxed pair" $
      out (flat (spacedBracket 0 0 "(#" "#)" (commaSep TrailingCommas [txt "a", txt "b"])))
        `shouldBe` "(# a, b #)\n"
    it "gives a spaced pair its own lines when broken" $
      out (broken (spacedBracket 0 0 "(#" "#)" (commaSep TrailingCommas [txt "a", txt "b"])))
        `shouldBe` "(#\n" <> step 1 <> "a,\n" <> step 1 <> "b\n#)\n"
    it "wraps in backticks" $
      out (flat (backticks (txt "div"))) `shouldBe` "`div`\n"

  describe "attach" $ do
    it "hands over the line when hanging" $
      out (broken (txt "=" <> attach Hanging (txt "do" >+> hardBreak <> txt "s")))
        `shouldBe` "= do\n" <> step 1 <> "s\n"
    it "breaks when normal" $
      out (broken (txt "=" >+> attach Normal (txt "f" <> space <> txt "x")))
        `shouldBe` "=\n" <> step 1 <> "f x\n"
    it "stays on one line when flat, either way" $ do
      out (flat (txt "=" <> attach Normal (txt "x"))) `shouldBe` "= x\n"
      out (flat (txt "=" <> attach Hanging (txt "x"))) `shouldBe` "= x\n"

  describe "relations" $ do
    it "begins a line a step right of the line the part before begins on" $
      out (broken (txt "f" <> space <> (txt "g" >+> hardBreak <> txt "x")))
        `shouldBe` "f g\n" <> step 1 <> "x\n"
    it "takes a step from the line the part before begins" $
      out (broken (txt "f" >+> hardBreak <> (txt "g" >+> hardBreak <> txt "x")))
        `shouldBe` "f\n" <> step 1 <> "g\n" <> step 2 <> "x\n"
    it "lines up what takes no step there" $
      out
        ( broken
            ( txt "|"
                <> DAligned 3 (txt "   " <> (txt "g" >+> hardBreak <> txt "x"))
            )
        )
        `shouldBe` "|   g\n    " <> step 1 <> "x\n"

  describe "inclusion" $ do
    it "includes when the condition holds" $
      out (flat (txt "a" <> includeWhen True (space <> txt "b")))
        `shouldBe` "a b\n"
    it "omits when it does not" $
      out (flat (txt "a" <> includeWhen False (space <> txt "b")))
        `shouldBe` "a\n"
    it "inverts with includeUnless" $
      out (flat (includeUnless True (txt "a"))) `shouldBe` ""

-- | Items in square brackets, with leading commas, each written on a line
-- of its own.
ledByCommas ::
  -- | Where the closing bracket goes.
  Int ->
  -- | The items.
  [Doc] ->
  Doc
ledByCommas closing =
  commaBracketWith LeadingCommas closing "[" "]"
    . commaSep LeadingCommas
    . zipWith (\l -> located (mkSpan (l, 3) (l, 4))) [1 ..]
