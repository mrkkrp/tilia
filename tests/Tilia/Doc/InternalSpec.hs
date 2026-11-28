{-# LANGUAGE OverloadedStrings #-}

-- | The rendering engine's primitives.
module Tilia.Doc.InternalSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Tilia.Doc
import Tilia.Doc.Combinators
import Tilia.Span
import Tilia.TestUtils (indentedBy, withEachSettings)

spec :: Spec
spec = withEachSettings $ \settings -> do
  let out = printDoc settings
      step = indentedBy settings
  describe "atoms" $ do
    it "renders nothing for an empty document" $
      out mempty `shouldBe` ""
    it "terminates output with a newline" $
      out (txt "x") `shouldBe` "x\n"
    it "collapses repeated spaces" $
      out (txt "a" <> space <> space <> txt "b") `shouldBe` "a b\n"
    it "drops a space before a line break" $
      out (txt "a" <> space <> hardBreak <> txt "b") `shouldBe` "a\nb\n"
    it "drops a leading space" $
      out (space <> txt "a") `shouldBe` "a\n"
    it "ignores an empty fragment" $
      out (txt "a" <> txt "" <> txt "b") `shouldBe` "ab\n"

  describe "blank lines" $ do
    it "collapses runs" $
      out (txt "a" <> blankLine <> blankLine <> txt "b")
        `shouldBe` "a\n\nb\n"
    it "drops a leading blank line" $
      out (blankLine <> txt "a") `shouldBe` "a\n"
    it "drops a trailing blank line" $
      out (txt "a" <> blankLine) `shouldBe` "a\n"
    it "separates when there is content on both sides" $
      out (txt "a" <> blankLine <> txt "b") `shouldBe` "a\n\nb\n"
    it "caps a run of hard breaks at one blank line" $ do
      out (txt "a" <> hardBreak <> hardBreak <> txt "b")
        `shouldBe` "a\n\nb\n"
      out (txt "a" <> hardBreak <> hardBreak <> hardBreak <> txt "b")
        `shouldBe` "a\n\nb\n"
      out (txt "a" <> mconcat (replicate 8 hardBreak) <> txt "b")
        `shouldBe` "a\n\nb\n"
    it "caps a mixture of hard breaks and blank lines" $
      out (txt "a" <> hardBreak <> blankLine <> hardBreak <> blankLine <> txt "b")
        `shouldBe` "a\n\nb\n"
    it "still breaks once for a single hard break" $
      out (txt "a" <> hardBreak <> txt "b") `shouldBe` "a\nb\n"

  describe "indentation" $ do
    it "indents by one step" $
      out (broken (txt "a" >+> breakOrSpace <> txt "b"))
        `shouldBe` "a\n" <> step 1 <> "b\n"
    it "nests relative to the enclosing level" $
      out (broken (txt "a" >+> breakOrSpace <> (txt "b" >+> breakOrSpace <> txt "c")))
        `shouldBe` "a\n" <> step 1 <> "b\n" <> step 2 <> "c\n"
    it "aligns to the current column" $
      out (broken (txt "ab" <> space <> align (txt "c" <> breakOrSpace <> txt "d")))
        `shouldBe` "ab c\n   d\n"
    it "leaves no trailing whitespace on an empty line" $
      out (broken (stepIn (txt "a" <> hardBreak <> hardBreak <> txt "b")))
        `shouldBe` step 1 <> "a\n\n" <> step 1 <> "b\n"
    it "does not indent a line with nothing on it" $
      out (broken (stepIn (txt "a" <> hardBreak)))
        `shouldBe` step 1 <> "a\n"
    it "puts held-back text that spills onto lines of its own at the line's indentation" $
      out (stepIn (txt "  a" <> held "-- x" <> held "-- y" <> hardBreak))
        `shouldBe` step 1 <> "  a -- x\n" <> step 1 <> "-- y\n"
    it "lines spilled held-back text up with what was held back before it" $
      out (stepIn (txt "  a" <> held "-- x" <> underPrevious "-- y" <> hardBreak))
        `shouldBe` step 1 <> "  a -- x\n" <> step 1 <> "    -- y\n"
    it "lines it up with what was held back before it where that spilled too" $
      out (stepIn (txt "  a" <> held "-- x" <> held "-- y" <> underPrevious "-- z" <> hardBreak))
        `shouldBe` step 1
          <> "  a -- x\n"
          <> step 1
          <> "-- y\n"
          <> step 1
          <> "-- z\n"

  describe "alignments" $ do
    it "begin their lines at the column they begin at" $
      out (txt "ab" <> space <> DAligned 0 (txt "c" <> hardBreak <> txt "d"))
        `shouldBe` "ab c\n   d\n"
    it "begin their lines so many columns right of that" $
      out (txt "ab" <> space <> DAligned 1 (txt "c" <> hardBreak <> txt "d"))
        `shouldBe` "ab c\n    d\n"

  describe "margin notes" $ do
    it "go to the margin right above a directive" $
      out (stepIn (txt "a" <> hardBreak <> DCppMarginNote (txt "-- x") <> hardBreak <> directive))
        `shouldBe` step 1 <> "a\n-- x\n#if X\n"
    it "go to the margin together when several are right above a directive" $
      out (stepIn (DCppMarginNote (txt "-- x") <> hardBreak <> DCppMarginNote (txt "-- y") <> hardBreak <> directive))
        `shouldBe` "-- x\n-- y\n#if X\n"
    it "stay indented when a blank line is between them and a directive" $
      out (stepIn (DCppMarginNote (txt "-- x") <> blankLine <> directive))
        `shouldBe` step 1 <> "-- x\n\n#if X\n"
    it "stay indented with no directive under them" $
      out (stepIn (DCppMarginNote (txt "-- x") <> hardBreak <> txt "a"))
        `shouldBe` step 1 <> "-- x\n" <> step 1 <> "a\n"
    it "stay where they are on a line something else began" $
      out (stepIn (txt "a" <> space <> DCppMarginNote (txt "-- x") <> hardBreak <> directive))
        `shouldBe` step 1 <> "a -- x\n#if X\n"
    it "stay indented when something else follows them on their line" $
      out (stepIn (DCppMarginNote (txt "{- x -}") <> space <> txt "a" <> hardBreak <> directive))
        `shouldBe` step 1 <> "{- x -} a\n#if X\n"

-- | A step in from an empty part placed right before it, so from the
-- indentation in force.
stepIn :: Doc -> Doc
stepIn d = ref mempty <> indent 1 (ptr mempty) d

held, underPrevious :: Text -> Doc
held = DComment (HeldBack SpillAtIndentation) (mkSpan (1, 1) (1, 1)) . DText
underPrevious =
  DComment (HeldBack SpillUnderPrevious) (mkSpan (1, 1) (1, 1)) . DText

directive :: Doc
directive = DCppDirective (mkSpan (9, 1) (9, 6)) "if X"
