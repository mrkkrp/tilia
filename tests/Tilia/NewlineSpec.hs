{-# LANGUAGE OverloadedStrings #-}

-- | How a text ends its lines.
module Tilia.NewlineSpec (spec) where

import Test.Hspec
import Tilia.Newline

spec :: Spec
spec = do
  describe "the style a text is written in" $ do
    it "is read off the first ending there is" $
      fmap getNewlineStyle ["a\r\nb\n", "a\nb\r\n", "a\nb\n", "a\r\nb\r\n"]
        `shouldBe` [CrLf, Lf, Lf, CrLf]

    it "is newlines for a text that ends no line at all" $
      getNewlineStyle "module A where" `shouldBe` Lf

    it "is newlines for a text with nothing in it" $
      getNewlineStyle "" `shouldBe` Lf

  describe "setting the style" $ do
    it "goes either way round" $ do
      setNewlineStyle CrLf "a\nb\n" `shouldBe` "a\r\nb\r\n"
      setNewlineStyle Lf "a\r\nb\r\n" `shouldBe` "a\nb\n"

    it "can be told to set the style already there, and change nothing" $
      fmap (\(style, t) -> setNewlineStyle style t) [(CrLf, "a\r\nb\r\n"), (Lf, "a\nb\n")]
        `shouldBe` ["a\r\nb\r\n", "a\nb\n"]

    it "brings a text whose endings disagree to one of them" $ do
      setNewlineStyle CrLf "a\r\nb\nc\r\n" `shouldBe` "a\r\nb\r\nc\r\n"
      setNewlineStyle Lf "a\r\nb\nc\r\n" `shouldBe` "a\nb\nc\n"

    it "leaves a carriage return that ends no line where it is" $ do
      setNewlineStyle Lf "x = \"a\\\r b\"\r\n" `shouldBe` "x = \"a\\\r b\"\n"
      setNewlineStyle CrLf "x = \"a\\\r b\"\n" `shouldBe` "x = \"a\\\r b\"\r\n"

    it "has nothing to do to a text that ends no line" $
      fmap (`setNewlineStyle` "module A where") [Lf, CrLf]
        `shouldBe` ["module A where", "module A where"]

    it "takes a text out and back unchanged, whichever style it began in" $
      [setNewlineStyle style (setNewlineStyle Lf t) | (style, t) <- [(CrLf, crlf), (Lf, lf)]]
        `shouldBe` [crlf, lf]
  where
    crlf = "module A where\r\n\r\nf :: Int\r\n"
    lf = "module A where\n\nf :: Int\n"
