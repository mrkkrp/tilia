{-# LANGUAGE OverloadedStrings #-}

-- | What a module's pragmas say, read off the text.
module Tilia.PragmaSpec (spec) where

import Data.Map.Strict (Map)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Driver.Session
  ( Language (..),
    OnOff (..),
    extensionFlags,
    fLangFlags,
    flagSpecName,
    glasgowExtsFlags,
    languageExtensions,
    xFlags,
  )
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Tilia.Pragma
import Tilia.Pragma.Ghc (InForce (..), inForceAfter, readFlags)

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
          ("one among pragmas that do not move anything", langThenLine),
          ( "one after a character literal holding a double quote",
            "module M where\nq = '\"'\n{-# LINE 9 \"O.hs\" #-}\n"
          ),
          ( "one after a quasi-quotation",
            "module M where\nx = [q|a|]\n{-# LINE 9 \"O.hs\" #-}\n"
          ),
          ("a line marker", "module M where\nx = 1\n# 40 \"O.hs\"\ny = x\n"),
          ("one written without a space", "module M where\n#40 \"O.hs\"\n"),
          ("one written as a #line", "module M where\n#line 40 \"O.hs\"\n"),
          ("one on the first line", "# 1 \"M.hs\"\nmodule M where\n"),
          ("one under a script's #! line", "#!/bin/runghc\n# 1 \"M.hs\"\n"),
          ("one after a comment", "module M where\n-- x\n# 9 \"O.hs\"\n"),
          ("a #pragma line", "module M where\n#pragma once\n"),
          ("a #! line below the top", "module M where\n#!/bin/sh\n"),
          ("a #! line after a space", " #!/bin/sh\nmodule M where\n")
        ]
        (\source -> movesPositions [QuasiQuotes] source `shouldBe` True)

      it "one after a list comprehension, where quasi-quotes are off" $
        movesPositions
          []
          "module M where\nxs = [x|x<-[1]]\n{-# LINE 9 \"O.hs\" #-}\n"
          `shouldBe` True

      it "a line marker written with a number where CPP is on, which cpp passes on" $
        movesPositions [Cpp] "module M where\n# 9 \"O.hs\"\n"
          `shouldBe` True

      it "a #! line below the top where CPP is on, which cpp passes on" $
        movesPositions [Cpp] "module M where\n#!/bin/sh\n"
          `shouldBe` True

    describe "says nothing of" $ do
      for_'
        [ ("a module with no pragma at all", "module M where\nx = 1\n"),
          ("a LANGUAGE pragma", "{-# LANGUAGE LambdaCase #-}\nmodule M where\n"),
          ("an INLINE pragma", "module M where\n{-# INLINE f #-}\nf = id\n"),
          ("a pragma whose name merely starts with one", "{-# LINEAR 1 #-}\n"),
          ("an unclosed pragma", "{-# LINE 1 \"Other.hs\"\n"),
          ("the word in a comment", "-- {-* LINE 1 *-}\nmodule M where\n"),
          ("one in a string", "module M where\nx = \"{-# LINE 1 #-}\"\n"),
          ( "one in a string after a character literal holding a double quote",
            "module M where\nq = '\"'\nx = \"{-# LINE 1 #-}\"\n"
          ),
          ( "one in a string after a character literal holding an escaped one",
            "module M where\nq = '\\\"'\nx = \"{-# LINE 1 #-}\"\n"
          ),
          ( "one in a string after a name ending in a prime",
            "module M where\nx = f'\"'\"\ny = \"{-# LINE 1 #-}\"\n"
          ),
          ( "one in a quasi-quotation",
            "module M where\nx = [q|\n{-# LINE 1 \"O.hs\" #-}\n|]\n"
          ),
          ( "one in a qualified quasi-quotation",
            "module M where\nx = [Q.q|{-# LINE 1 \"O.hs\" #-}|]\n"
          ),
          ("the #! lines a script begins with", "#!/bin/sh\n#!x\n-- stack\n"),
          ( "lines GHC reads past, in a quasi-quotation",
            "module M where\nx = [q|\n# 9 \"O.hs\"\n#pragma once\n#!/bin/sh\n|]\n"
          ),
          ("a hash and a number in a block comment", "{-\n#15888 says\n-}\n"),
          ("a label opening an indented line", "module M where\nf = g\n  #line\n")
        ]
        (\source -> movesPositions [QuasiQuotes] source `shouldBe` False)

      it "a line marker where CPP is on, which cpp reads" $
        movesPositions [Cpp] "module M where\n#line 9 \"O.hs\"\n"
          `shouldBe` False

      it "a #pragma line where CPP is on, which cpp reads" $
        movesPositions [Cpp] "module M where\n#pragma once\n"
          `shouldBe` False
  describe "the extensions in force" $ do
    it "keeps what the package puts in force" $
      inForceUnder [ImplicitPrelude, GADTs] "module M where\n"
        `shouldBe` [ImplicitPrelude, GADTs]
    it "reads one extension" $
      inForceUnder [] "{-# LANGUAGE BangPatterns #-}\nmodule M where\n"
        `shouldBe` [BangPatterns]
    it "reads several from one pragma" $
      inForceUnder
        []
        "{-# LANGUAGE LambdaCase, MagicHash #-}\nmodule M where\n"
        `shouldBe` [LambdaCase, MagicHash]
    it "reads several pragmas" $
      inForceUnder
        []
        "{-# LANGUAGE LambdaCase #-}\n{-# LANGUAGE MagicHash #-}\n"
        `shouldBe` [LambdaCase, MagicHash]
    it "ignores flags that turn on no extension" $
      inForceUnder
        []
        "{-# OPTIONS_GHC -Wall #-}\n{-# LANGUAGE LambdaCase #-}\n"
        `shouldBe` [LambdaCase]
    it "reads the -X flags of OPTIONS_GHC" $
      inForceUnder
        [GADTs]
        "{-# OPTIONS_GHC -Wall -XLambdaCase -XNoGADTs #-}\nmodule M where\n"
        `shouldBe` [LambdaCase]
    it "reads the flags of OPTIONS" $
      inForceUnder [] "{-# OPTIONS -XLambdaCase #-}\n"
        `shouldBe` [LambdaCase]
    it "reads the flags of an options pragma as GHC splits them" $
      inForceUnder [] "{-# OPTIONS_GHC \"-XLambdaCase\" #-}\n"
        `shouldBe` [LambdaCase]
    it "reads -cpp as turning on CPP" $
      inForceUnder [] "{-# OPTIONS_GHC -cpp #-}\n"
        `shouldBe` [Cpp]
    it "reads the old -f spellings of extensions" $
      inForceUnder
        [ImplicitPrelude]
        "{-# OPTIONS_GHC -fbang-patterns -fno-implicit-prelude #-}\n"
        `shouldBe` [BangPatterns]
    it "reads -fglasgow-exts as turning on what it stands for" $
      inForceUnder [] "{-# OPTIONS_GHC -fglasgow-exts #-}\n"
        `shouldSatisfy` \es -> all (`elem` es) glasgowExtsFlags
    it "reads -fno-glasgow-exts as turning that off again" $
      inForceUnder
        []
        "{-# OPTIONS_GHC -fglasgow-exts -fno-glasgow-exts #-}\n"
        `shouldSatisfy` not . any (`elem` glasgowExtsFlags)
    it "reads the pragmas in the order they are written" $
      inForceUnder
        []
        ( "{-# LANGUAGE LambdaCase #-}\n"
            <> "{-# OPTIONS_GHC -XNoLambdaCase -XMagicHash #-}\n"
            <> "{-# LANGUAGE NoMagicHash #-}\n"
        )
        `shouldBe` []
    it "reads the names of a language pragma past the comments in it" $
      inForceUnder
        []
        "{-# LANGUAGE\n    LambdaCase -- for f\n  , {- c -} MagicHash\n  #-}\nmodule M where\n"
        `shouldBe` [LambdaCase, MagicHash]
    it "ignores a pragma below the module line, which GHC does not read" $
      inForceUnder [] "module M where\n{-# LANGUAGE LambdaCase #-}\n"
        `shouldBe` []
    it "reads the pragmas between the directives of a header" $
      inForceUnder
        []
        "{-# LANGUAGE CPP #-}\n#if 1\n{-# LANGUAGE LambdaCase #-}\n#endif\nmodule M where\n"
        `shouldBe` [Cpp, LambdaCase]
    it "takes an edition the module names in place of the component's" $
      inForceUnder
        (languageExtensions (Just Haskell2010))
        "{-# LANGUAGE GHC2024 #-}\nmodule M where\n"
        `shouldBe` languageExtensions (Just GHC2024)
    it "keeps what the component switches on top of the module's edition" $
      effectiveExtensions
        (defaultsOf (languageExtensions (Just Haskell2010)) [On OverloadedStrings])
        "{-# LANGUAGE GHC2021 #-}\nmodule M where\n"
        `shouldSatisfy` \es -> all (`elem` es) [OverloadedStrings, ImportQualifiedPost]
    it "switches what the module switches on top of its edition, wherever named" $
      inForceUnder
        []
        "{-# LANGUAGE NoLambdaCase #-}\n{-# LANGUAGE GHC2024 #-}\nmodule M where\n"
        `shouldSatisfy` notElem LambdaCase
    it "takes the edition named last" $
      inForceUnder
        []
        "{-# LANGUAGE GHC2024 #-}\n{-# LANGUAGE Haskell2010 #-}\nmodule M where\n"
        `shouldBe` languageExtensions (Just Haskell2010)
    it "reads an edition from a -X flag" $
      inForceUnder [] "{-# OPTIONS_GHC -XGHC2024 #-}\nmodule M where\n"
        `shouldBe` languageExtensions (Just GHC2024)
    it "ignores the options of other compilers" $
      inForceUnder [] "{-# OPTIONS_JHC -fffi -cpp #-}\n"
        `shouldBe` []
    it "ignores an unknown extension rather than failing" $
      inForceUnder [] "{-# LANGUAGE LambdaCase, NotARealExtension #-}\n"
        `shouldBe` [LambdaCase]
    it "treats a No-prefix as turning one off" $
      inForceUnder
        [ImplicitPrelude]
        "{-# LANGUAGE NoImplicitPrelude #-}\n"
        `shouldBe` []
    it "lets a module refuse what its package put in force" $
      inForceUnder [ImplicitPrelude, GADTs] "{-# LANGUAGE NoGADTs #-}\n"
        `shouldBe` [ImplicitPrelude]
    it "lets a module take back a Prelude its package turned off" $
      inForceUnder [GADTs] "{-# LANGUAGE ImplicitPrelude #-}\n"
        `shouldBe` [GADTs, ImplicitPrelude]
    it "does not repeat an extension named twice" $
      inForceUnder
        []
        "{-# LANGUAGE LambdaCase #-}\n{-# LANGUAGE LambdaCase #-}\n"
        `shouldBe` [LambdaCase]
    it "turns on what an extension implies, as GHC does" $
      inForceUnder [] "{-# LANGUAGE RankNTypes #-}\n"
        `shouldBe` [RankNTypes, ExplicitForAll]
    it "turns off what an extension implies is off" $
      inForceUnder
        [ImplicitPrelude]
        "{-# LANGUAGE RebindableSyntax #-}\n"
        `shouldBe` [RebindableSyntax]
    it "leaves on what an extension turned off had implied" $
      inForceUnder [] "{-# LANGUAGE RankNTypes, NoRankNTypes #-}\n"
        `shouldBe` [ExplicitForAll]
    it "lets a later name turn off what an earlier one implied" $
      inForceUnder [] "{-# LANGUAGE RankNTypes, NoExplicitForAll #-}\n"
        `shouldBe` [RankNTypes]

  modifyMaxSuccess (const 5000) . describe "against GHC's own flag reader" $ do
    it "leaves on what GHC leaves on" $
      forAll (listOf1 (pragmaOf headerNames)) $ \pragmas ->
        let ours =
              effectiveExtensions
                (defaultsOf (languageExtensions Nothing) [])
                (headerOf pragmas)
            theirs =
              EnumSet.toList (extensionFlags (readFlags (concatMap flagsOf pragmas)))
         in counterexample (T.unpack (headerOf pragmas)) $
              Set.fromList ours === Set.fromList theirs
    it "takes two orders to be alike where GHC switches alike" $
      forAll (listOf1 (pragmaOf extensionNames)) $ \pragmas ->
        forAll (shuffle pragmas) $ \shuffled ->
          counterexample (T.unpack (headerOf pragmas <> headerOf shuffled)) $
            sameInForce (fmap namedBody pragmas) (fmap namedBody shuffled)
              === (ghcSwitches pragmas == ghcSwitches shuffled)

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

-- | The extensions in force in a module of a component whose edition puts
-- these in force and which switches nothing itself.
inForceUnder :: [Extension] -> Text -> [Extension]
inForceUnder edition = effectiveExtensions (defaultsOf edition [])

-- | A pragma of a header, as its name and its body, with the flags it hands
-- GHC.
type HeaderPragma = (Text, Text, [Text])

-- | A pragma that names one of these, or spells an extension as an old flag.
pragmaOf :: [Text] -> Gen HeaderPragma
pragmaOf names = oneof [language, options]
  where
    language = do
      name <- elements names
      pure ("LANGUAGE", name, ["-X" <> name])
    options = do
      flag <- elements oldFlags
      pure ("OPTIONS_GHC", flag, [flag])
    oldFlags =
      ["-cpp", "-fglasgow-exts", "-fno-glasgow-exts"]
        <> concat
          [ ["-f" <> name, "-fno-" <> name]
          | name <- T.pack . flagSpecName <$> fLangFlags
          ]

-- | Every name of an extension, with @No@ in front and without.
extensionNames :: [Text]
extensionNames = names <> fmap ("No" <>) names
  where
    names = T.pack . flagSpecName <$> xFlags

-- | Every name a @LANGUAGE@ pragma can hold: the extensions and the editions.
headerNames :: [Text]
headerNames =
  extensionNames <> [T.pack (show l) | l <- [minBound .. maxBound :: Language]]

-- | A module of nothing but a header of these pragmas.
headerOf :: [HeaderPragma] -> Text
headerOf pragmas =
  T.concat ["{-# " <> n <> " " <> b <> " #-}\n" | (n, b, _) <- pragmas]
    <> "module M where\n"

flagsOf :: HeaderPragma -> [Text]
flagsOf (_, _, flags) = flags

namedBody :: HeaderPragma -> (Text, Text)
namedBody (n, b, _) = (n, b)

-- | How GHC takes these pragmas to switch each extension.
ghcSwitches :: [HeaderPragma] -> Map Extension Bool
ghcSwitches = inForceSwitches . inForceAfter . concatMap flagsOf
