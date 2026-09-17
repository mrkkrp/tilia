{-# LANGUAGE OverloadedStrings #-}

-- | What a build plan settles about a module's conditionals.
module Tilia.Cpp.MacrosSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Cpp (withoutRuledOut)
import Tilia.Cpp.Macros

-- | A plan with one dependency at 1.2.3 and a compiler at 9.10.3.
macros :: Macros
macros =
  Macros
    { macroVersions =
        Map.fromList
          [ ("MIN_VERSION_thing", [1, 2, 3]),
            ("MIN_VERSION_GLASGOW_HASKELL", [9, 10, 3, 0])
          ],
      macroNumbers =
        Map.fromList
          [ ("__GLASGOW_HASKELL__", 910),
            ("__GLASGOW_HASKELL_PATCHLEVEL1__", 3),
            ("__GLASGOW_HASKELL_PATCHLEVEL2__", 0)
          ]
    }

-- | What this guard comes to, given the plan above.
answer :: Text -> Maybe Bool
answer = guardHolds macros

spec :: Spec
spec = do
  describe "a guard about a version the plan fixed" $ do
    it "is true where the plan is at least what it asks for" $
      map answer ["if MIN_VERSION_thing(1,2,3)", "if MIN_VERSION_thing(1,0,0)"]
        `shouldBe` [Just True, Just True]

    it "is false where the plan is short of it" $
      map answer ["if MIN_VERSION_thing(1,2,4)", "if MIN_VERSION_thing(2,0,0)"]
        `shouldBe` [Just False, Just False]

    it "compares the parts as numbers and not as text" $
      answer "if MIN_VERSION_thing(1,10,0)" `shouldBe` Just False

    it "pads the shorter side with zeros" $
      map answer ["if MIN_VERSION_thing(1,2)", "if MIN_VERSION_thing(1,2,3,1)"]
        `shouldBe` [Just True, Just False]

    it "says nothing about a package the plan does not name" $
      answer "if MIN_VERSION_other(1,0,0)" `shouldBe` Nothing

  describe "a guard about the compiler" $ do
    it "reads its version as the compiler spells it" $
      map answer ["if __GLASGOW_HASKELL__ >= 902", "if __GLASGOW_HASKELL__ >= 912"]
        `shouldBe` [Just True, Just False]

    it "takes the four-part macro apart the same way" $
      map
        answer
        [ "if MIN_VERSION_GLASGOW_HASKELL(9,10,1,0)",
          "if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)",
          "if MIN_VERSION_GLASGOW_HASKELL(9,12,1,0)"
        ]
        `shouldBe` [Just True, Just True, Just False]

  describe "an answer that needs more than one question settled" $ do
    it "carries a false through a conjunction whatever else is in it" $
      answer "if defined(SOMETHING) && MIN_VERSION_thing(2,0,0)"
        `shouldBe` Just False

    it "carries a true through a disjunction the same way" $
      answer "if defined(SOMETHING) || MIN_VERSION_thing(1,0,0)"
        `shouldBe` Just True

    it "gives up where what is left over decides it" $
      map
        answer
        [ "if defined(SOMETHING) && MIN_VERSION_thing(1,0,0)",
          "if defined(SOMETHING) || MIN_VERSION_thing(2,0,0)"
        ]
        `shouldBe` [Nothing, Nothing]

    it "answers a version test behind a defined of the same macro" $
      answer "if defined(MIN_VERSION_thing) && MIN_VERSION_thing(1,0,0)"
        `shouldBe` Just True

    it "negates what it knows and nothing else" $
      map answer ["if !MIN_VERSION_thing(2,0,0)", "if !defined(SOMETHING)"]
        `shouldBe` [Just True, Nothing]

    it "reads brackets" $
      answer "if (MIN_VERSION_thing(1,0,0) || defined(X)) && !MIN_VERSION_thing(9,0,0)"
        `shouldBe` Just True

  describe "a guard that is not about a version at all" $ do
    it "answers a bare number, which is how a branch is turned off" $
      map answer ["if 0", "if 1"] `shouldBe` [Just False, Just True]

    it "says nothing about a flag" $
      map answer ["ifdef FOO", "ifndef FOO", "if defined FOO"]
        `shouldBe` [Nothing, Nothing, Nothing]

    it "says a macro it has a value for is defined" $
      map answer ["ifdef MIN_VERSION_thing", "ifndef MIN_VERSION_thing"]
        `shouldBe` [Just True, Just False]

    it "says nothing about arithmetic, which it does not read" $
      answer "if __GLASGOW_HASKELL__ + 1 > 900" `shouldBe` Nothing

    it "says nothing about a guard whose keyword asks nothing" $
      map answer ["else", "endif", "define FOO 1"]
        `shouldBe` [Nothing, Nothing, Nothing]

  describe "blanking the branches a plan rules out" $ do
    it "leaves the taken branch and blanks the rest" $
      ruledOut
        [ "#if MIN_VERSION_thing(1,0,0)",
          "import New",
          "#else",
          "import Old",
          "#endif"
        ]
        `shouldBe` ["", "import New", "", "", ""]

    it "takes the #else where the condition fails" $
      ruledOut
        [ "#if MIN_VERSION_thing(2,0,0)",
          "import New",
          "#else",
          "import Old",
          "#endif"
        ]
        `shouldBe` ["", "", "", "import Old", ""]

    it "leaves nothing where the condition fails and there is no #else" $
      ruledOut
        [ "#if MIN_VERSION_thing(2,0,0)",
          "import New",
          "#endif"
        ]
        `shouldBe` ["", "", ""]

    it "takes the first branch of an #elif chain that holds" $
      ruledOut
        [ "#if MIN_VERSION_thing(2,0,0)",
          "import Newest",
          "#elif MIN_VERSION_thing(1,0,0)",
          "import New",
          "#else",
          "import Old",
          "#endif"
        ]
        `shouldBe` ["", "", "", "import New", "", "", ""]

    it "leaves a conditional it cannot answer exactly as it was" $
      ruledOut
        [ "#ifdef FOO",
          "import One",
          "#else",
          "import Two",
          "#endif"
        ]
        `shouldBe` ["#ifdef FOO", "import One", "#else", "import Two", "#endif"]

    it "leaves a chain alone from the first question it cannot answer" $
      ruledOut
        [ "#if defined(FOO)",
          "import One",
          "#elif MIN_VERSION_thing(1,0,0)",
          "import Two",
          "#endif"
        ]
        `shouldBe` ["#if defined(FOO)", "import One", "#elif MIN_VERSION_thing(1,0,0)", "import Two", "#endif"]

    it "leaves the branches before a question it cannot answer as well" $
      ruledOut
        [ "#if MIN_VERSION_thing(2,0,0)",
          "import One",
          "#elif defined(FOO)",
          "import Two",
          "#endif"
        ]
        `shouldBe` [ "#if MIN_VERSION_thing(2,0,0)",
                     "import One",
                     "#elif defined(FOO)",
                     "import Two",
                     "#endif"
                   ]

    it "rules out a shim defining a macro the plan already has" $
      ruledOut
        [ "#ifndef MIN_VERSION_thing",
          "#define MIN_VERSION_thing(a,b,c) 1",
          "#endif"
        ]
        `shouldBe` ["", "", ""]

    it "reaches a conditional nested inside the branch that is taken" $
      ruledOut
        [ "#if MIN_VERSION_thing(1,0,0)",
          "#if MIN_VERSION_thing(2,0,0)",
          "import Newest",
          "#else",
          "import New",
          "#endif",
          "#endif"
        ]
        `shouldBe` ["", "", "", "", "import New", "", ""]

    it "keeps every line where it was written" $
      let source = T.unlines ["module M where", "#if MIN_VERSION_thing(2,0,0)", "x = 1", "#endif"]
       in length (T.lines (withoutRuledOut macros source))
            `shouldBe` length (T.lines source)

    it "leaves a module with no conditionals in it alone" $
      withoutRuledOut macros "module M where\n" `shouldBe` "module M where\n"

-- | The lines of a module once the plan has ruled out what it can.
ruledOut :: [Text] -> [Text]
ruledOut = T.lines . withoutRuledOut macros . T.unlines
