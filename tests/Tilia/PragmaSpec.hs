{-# LANGUAGE OverloadedStrings #-}

-- | What a module's pragmas say, read off the text.
module Tilia.PragmaSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import GHC.LanguageExtensions.Type (Extension (..))
import Tilia.Pragma

spec :: Spec
spec = do
  describe "movesPositions" $ do
    describe "says so" $ do
      for_'
        [ ("a LINE pragma", "{-# LINE 1 \"Other.hs\" #-}\n"),
          ("a COLUMN pragma", "{-# COLUMN 20 #-}\n"),
          ("one written in lower case", "{-# line 1 \"Other.hs\" #-}\n"),
          ("one written without spaces", "{-#LINE 1 \"Other.hs\"#-}\n"),
          ("one written over several lines", "{-# LINE 1\n      \"Other.hs\" #-}\n"),
          ("one below the header", "module M where\nx = 1\n{-# LINE 9 \"O.hs\" #-}\n"),
          ("one among pragmas that do not move anything", langThenLine)
        ]
        (\source -> movesPositions source `shouldBe` True)

    describe "says nothing of" $ do
      for_'
        [ ("a module with no pragma at all", "module M where\nx = 1\n"),
          ("a LANGUAGE pragma", "{-# LANGUAGE LambdaCase #-}\nmodule M where\n"),
          ("an INLINE pragma", "module M where\n{-# INLINE f #-}\nf = id\n"),
          ("a pragma whose name merely starts with one", "{-# LINEAR 1 #-}\n"),
          ("an unclosed pragma", "{-# LINE 1 \"Other.hs\"\n"),
          ("the word in a comment", "-- {-* LINE 1 *-}\nmodule M where\n")
        ]
        (\source -> movesPositions source `shouldBe` False)
  describe "the extensions in force" $ do
    it "starts from what is on by default" $
      effectiveExtensions [] "module M where\n" `shouldBe` [ImplicitPrelude]
    it "keeps what the package puts in force" $
      effectiveExtensions [GADTs] "module M where\n"
        `shouldBe` [ImplicitPrelude, GADTs]
    it "reads one extension" $
      effectiveExtensions [] "{-# LANGUAGE BangPatterns #-}\nmodule M where\n"
        `shouldBe` [ImplicitPrelude, BangPatterns]
    it "reads several from one pragma" $
      effectiveExtensions [] "{-# LANGUAGE GADTs, RankNTypes #-}\nmodule M where\n"
        `shouldBe` [ImplicitPrelude, GADTs, RankNTypes]
    it "reads several pragmas" $
      effectiveExtensions [] "{-# LANGUAGE GADTs #-}\n{-# LANGUAGE MagicHash #-}\n"
        `shouldBe` [ImplicitPrelude, GADTs, MagicHash]
    it "ignores other pragmas" $
      effectiveExtensions [] "{-# OPTIONS_GHC -Wall #-}\n{-# LANGUAGE GADTs #-}\n"
        `shouldBe` [ImplicitPrelude, GADTs]
    it "ignores an unknown extension rather than failing" $
      effectiveExtensions [] "{-# LANGUAGE GADTs, NotARealExtension #-}\n"
        `shouldBe` [ImplicitPrelude, GADTs]
    it "treats a No-prefix as turning one off" $
      effectiveExtensions [] "{-# LANGUAGE NoImplicitPrelude #-}\n" `shouldBe` []
    it "lets a module refuse what its package put in force" $
      effectiveExtensions [GADTs] "{-# LANGUAGE NoGADTs #-}\n"
        `shouldBe` [ImplicitPrelude]
    it "does not repeat an extension named twice" $
      effectiveExtensions [] "{-# LANGUAGE GADTs #-}\n{-# LANGUAGE GADTs #-}\n"
        `shouldBe` [ImplicitPrelude, GADTs]

  describe "lookupExtension" $ do
    it "knows an extension by the name one writes" $
      lookupExtension "LambdaCase" `shouldBe` Just LambdaCase
    it "says nothing of a name no compiler knows" $
      lookupExtension "NotARealExtension" `shouldBe` Nothing
    it "does not accept the No-prefixed spelling as a name" $
      lookupExtension "NoImplicitPrelude" `shouldBe` Nothing
  where
    for_' cases expect = mapM_ (\(what, source) -> it what (expect source)) cases

    langThenLine :: Text
    langThenLine = "{-# LANGUAGE LambdaCase #-}\nmodule M where\n{-# LINE 3 \"O.hs\" #-}\n"
