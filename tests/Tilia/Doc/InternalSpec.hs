{-# LANGUAGE OverloadedStrings #-}

-- | The span algebra and the rendering engine's primitives.
module Tilia.Doc.InternalSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Tilia.Doc
import Tilia.Doc.Combinators
import Tilia.Doc.Internal
  ( Doc (DCppDirective, DHoldBack),
    Spill (..),
  )
import Tilia.Span

spec :: Spec
spec = do
  describe "Span" $ do
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
      out (broken (txt "a" <> indent (breakOrSpace <> txt "b")))
        `shouldBe` "a\n  b\n"
    it "nests relative to the enclosing level" $
      out (broken (txt "a" <> indent (breakOrSpace <> txt "b" <> indent (breakOrSpace <> txt "c"))))
        `shouldBe` "a\n  b\n    c\n"
    it "aligns to the current column" $
      out (broken (txt "ab" <> space <> align (txt "c" <> breakOrSpace <> txt "d")))
        `shouldBe` "ab c\n   d\n"
    it "leaves no trailing whitespace on an empty line" $
      out (broken (indent (txt "a" <> hardBreak <> hardBreak <> txt "b")))
        `shouldBe` "  a\n\n  b\n"
    it "does not indent a line with nothing on it" $
      out (broken (indent (txt "a" <> hardBreak)))
        `shouldBe` "  a\n"
    it "puts held-back text that spills onto lines of its own at the line's indentation" $
      out (indent (txt "  a" <> held "-- x" <> held "-- y" <> hardBreak))
        `shouldBe` "    a -- x\n  -- y\n"
    it "lines spilled held-back text up with what was held back before it" $
      out (indent (txt "  a" <> held "-- x" <> underPrevious "-- y" <> hardBreak))
        `shouldBe` "    a -- x\n      -- y\n"
    it "lines it up with what was held back before it where that spilled too" $
      out (indent (txt "  a" <> held "-- x" <> held "-- y" <> underPrevious "-- z" <> hardBreak))
        `shouldBe` "    a -- x\n  -- y\n  -- z\n"

  describe "margin notes" $ do
    it "go to the margin right above a directive" $
      out (indent (txt "a" <> hardBreak <> cppMarginNote (txt "-- x") <> hardBreak <> directive))
        `shouldBe` "  a\n-- x\n#if X\n"
    it "go to the margin together when several are right above a directive" $
      out (indent (cppMarginNote (txt "-- x") <> hardBreak <> cppMarginNote (txt "-- y") <> hardBreak <> directive))
        `shouldBe` "-- x\n-- y\n#if X\n"
    it "stay indented when a blank line is between them and a directive" $
      out (indent (cppMarginNote (txt "-- x") <> blankLine <> directive))
        `shouldBe` "  -- x\n\n#if X\n"
    it "stay indented with no directive under them" $
      out (indent (cppMarginNote (txt "-- x") <> hardBreak <> txt "a"))
        `shouldBe` "  -- x\n  a\n"
    it "stay where they are on a line something else began" $
      out (indent (txt "a" <> space <> cppMarginNote (txt "-- x") <> hardBreak <> directive))
        `shouldBe` "  a -- x\n#if X\n"
    it "stay indented when something else follows them on their line" $
      out (indent (cppMarginNote (txt "{- x -}") <> space <> txt "a" <> hardBreak <> directive))
        `shouldBe` "  {- x -} a\n#if X\n"

held, underPrevious :: Text -> Doc
held = DHoldBack SpillAtIndentation
underPrevious = DHoldBack SpillUnderPrevious

out :: Doc -> Text
out = printDoc defaultRenderOptions

directive :: Doc
directive = DCppDirective (mkSpan (9, 1) (9, 6)) "if X"
