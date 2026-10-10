{-# LANGUAGE OverloadedStrings #-}

-- | What a build plan settles about a module's conditionals.
module Tilia.Cpp.MacrosSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Cpp.Directives (withoutRuledOut)
import Tilia.Cpp.Macros

-- | A plan with one dependency at 1.2.3 and GHC at 9.10.3.
macros :: Macros
macros =
  mempty
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
          ],
      macroUndefined = Set.fromList ["__MHS__", "__HUGS__"],
      macroPackages = Just (Set.fromList ["thing"])
    }

-- | What this guard comes to, given the plan above.
answer :: Text -> Maybe Bool
answer = guardHolds macros

spec :: Spec
spec = do
  describe "a guard about a version the plan fixed" $ do
    it "is true where the plan is at least what it asks for" $
      fmap answer ["if MIN_VERSION_thing(1,2,3)", "if MIN_VERSION_thing(1,0,0)"]
        `shouldBe` [Just True, Just True]

    it "is false where the plan is short of it" $
      fmap answer ["if MIN_VERSION_thing(1,2,4)", "if MIN_VERSION_thing(2,0,0)"]
        `shouldBe` [Just False, Just False]

    it "compares the parts as numbers and not as text" $
      answer "if MIN_VERSION_thing(1,10,0)" `shouldBe` Just False

    it "pads the shorter side with zeros" $
      fmap answer ["if MIN_VERSION_thing(1,2)", "if MIN_VERSION_thing(1,2,3,1)"]
        `shouldBe` [Just True, Just False]

    it "has the macros of a package the plan does not hold undefined" $
      fmap
        answer
        [ "ifdef VERSION_other",
          "ifndef VERSION_other",
          "if defined(MIN_VERSION_other)",
          "if MIN_VERSION_other(1,0,0)"
        ]
        `shouldBe` [Just False, Just True, Just False, Just False]

    it "says nothing about other packages where the plan's are unknown" $
      guardHolds macros{macroPackages = Nothing} "ifdef VERSION_other"
        `shouldBe` Nothing

  describe "a guard about the compiler" $ do
    it "reads its version as the compiler spells it" $
      fmap answer ["if __GLASGOW_HASKELL__ >= 902", "if __GLASGOW_HASKELL__ >= 912"]
        `shouldBe` [Just True, Just False]

    it "takes the four-part macro apart the same way" $
      fmap
        answer
        [ "if MIN_VERSION_GLASGOW_HASKELL(9,10,1,0)",
          "if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)",
          "if MIN_VERSION_GLASGOW_HASKELL(9,12,1,0)"
        ]
        `shouldBe` [Just True, Just True, Just False]

  describe "a guard about another compiler" $ do
    it "says its macros are not defined" $
      fmap
        answer
        [ "if defined(__MHS__)",
          "if defined __HUGS__",
          "ifdef __MHS__",
          "ifndef __HUGS__",
          "if !defined(__MHS__)"
        ]
        `shouldBe` [Just False, Just False, Just False, Just True, Just True]

    it "reads them as zero where a number is asked for" $
      fmap answer ["if __MHS__", "if __HUGS__ >= 200", "if __HUGS__ == 0"]
        `shouldBe` [Just False, Just False, Just True]

    it "answers a guard that names this compiler alongside" $
      fmap
        answer
        [ "if defined(__GLASGOW_HASKELL__) && !defined(__MHS__)",
          "if defined(__MHS__) || __GLASGOW_HASKELL__ >= 902"
        ]
        `shouldBe` [Just True, Just True]

    it "says nothing where a flag it does not know decides" $
      fmap answer ["if defined(__MHS__) || defined(FOO)", "if !defined(__MHS__) && defined(FOO)"]
        `shouldBe` [Nothing, Nothing]

  describe "an answer that needs more than one question settled" $ do
    it "carries a false through a conjunction whatever else is in it" $
      answer "if defined(SOMETHING) && MIN_VERSION_thing(2,0,0)"
        `shouldBe` Just False

    it "carries a true through a disjunction the same way" $
      answer "if defined(SOMETHING) || MIN_VERSION_thing(1,0,0)"
        `shouldBe` Just True

    it "gives up where what is left over decides it" $
      fmap
        answer
        [ "if defined(SOMETHING) && MIN_VERSION_thing(1,0,0)",
          "if defined(SOMETHING) || MIN_VERSION_thing(2,0,0)"
        ]
        `shouldBe` [Nothing, Nothing]

    it "answers a version test behind a defined of the same macro" $
      answer "if defined(MIN_VERSION_thing) && MIN_VERSION_thing(1,0,0)"
        `shouldBe` Just True

    it "negates what it knows and nothing else" $
      fmap answer ["if !MIN_VERSION_thing(2,0,0)", "if !defined(SOMETHING)"]
        `shouldBe` [Just True, Nothing]

    it "reads brackets" $
      answer "if (MIN_VERSION_thing(1,0,0) || defined(X)) && !MIN_VERSION_thing(9,0,0)"
        `shouldBe` Just True

  describe "a guard that is not about a version at all" $ do
    it "answers a bare number, which is how a branch is turned off" $
      fmap answer ["if 0", "if 1"] `shouldBe` [Just False, Just True]

    it "says nothing about a flag" $
      fmap answer ["ifdef FOO", "ifndef FOO", "if defined FOO"]
        `shouldBe` [Nothing, Nothing, Nothing]

    it "says a macro it has a value for is defined" $
      fmap answer ["ifdef MIN_VERSION_thing", "ifndef MIN_VERSION_thing"]
        `shouldBe` [Just True, Just False]

    it "says nothing about arithmetic, which it does not read" $
      answer "if __GLASGOW_HASKELL__ + 1 > 900" `shouldBe` Nothing

    it "says nothing about a guard whose keyword asks nothing" $
      fmap answer ["else", "endif", "define FOO 1"]
        `shouldBe` [Nothing, Nothing, Nothing]

  describe "a guard about a macro known to be defined or not" $ do
    let known = mempty{macroDefined = Set.fromList ["ON"], macroUndefined = Set.fromList ["OFF"]}
        knownAnswer = guardHolds known

    it "takes one that is not defined for 0" $
      fmap knownAnswer ["if OFF", "if !OFF", "if OFF > 1"]
        `shouldBe` [Just False, Just True, Just False]

    it "says whether each is defined" $
      fmap knownAnswer ["ifdef ON", "ifndef ON", "ifdef OFF", "if defined(OFF)"]
        `shouldBe` [Just True, Just False, Just False, Just False]

    it "says nothing about the value of one that is defined" $
      knownAnswer "if ON" `shouldBe` Nothing

  describe "what a guard implies about which macros are defined" $ do
    let defined n = mempty{macroDefined = Set.singleton n}
        notDefined n = mempty{macroUndefined = Set.singleton n}

    it "reads it off a guard asking whether one is" $
      fmap
        (impliedBy True)
        ["ifdef A", "ifndef A", "if defined(A)", "if defined A", "if !defined(A)"]
        `shouldBe` [ defined "A",
                     notDefined "A",
                     defined "A",
                     defined "A",
                     notDefined "A"
                   ]

    it "takes a failing guard to say the opposite" $
      fmap (impliedBy False) ["ifdef A", "if !defined(A)"]
        `shouldBe` [notDefined "A", defined "A"]

    it "takes a macro that is not 0 for one that is defined" $
      fmap (impliedBy True) ["if A", "elif A"] `shouldBe` [defined "A", defined "A"]

    it "learns nothing from a macro that is 0, which it may be by not being defined" $
      (impliedBy False "if A", impliedBy True "if !A") `shouldBe` (mempty, mempty)

    it "reads both sides of a conjunction that holds and of a disjunction that fails" $
      ( impliedBy True "if defined(A) && !defined(B)",
        impliedBy False "if defined(A) || defined(B)"
      )
        `shouldBe` (defined "A" <> notDefined "B", notDefined "A" <> notDefined "B")

    it "learns nothing from a conjunction that fails or a disjunction that holds" $
      (impliedBy False "if defined(A) && defined(B)", impliedBy True "if defined(A) || defined(B)")
        `shouldBe` (mempty, mempty)

    it "learns nothing from a call of a macro other than a version test" $
      fmap (impliedBy True) ["if CHECK(1,2)", "if MIN_VERSION_base(x)", "if 1"]
        `shouldBe` [mempty, mempty, mempty]

  describe "a guard under what other guards imply about versions" $ do
    let knownAnswer =
          guardHolds
            ( impliedBy True "if MIN_VERSION_base(4,10,0) && !(MIN_VERSION_base(4,12,0))"
                <> impliedBy True "if MIN_VERSION_base(4,9)"
            )

    it "is settled where the bounds they put on the version decide it" $
      fmap
        knownAnswer
        [ "if MIN_VERSION_base(4,9,0)",
          "if MIN_VERSION_base(4,10)",
          "if MIN_VERSION_base(4,12)",
          "if MIN_VERSION_base(5,0,0)",
          "if MIN_VERSION_base(4,11,0)"
        ]
        `shouldBe` [Just True, Just True, Just False, Just False, Nothing]

    it "reads each comparison of a macro with a number as bounds on it" $
      fmap
        ( \op ->
            fmap
              (guardHolds (impliedBy True ("if __GLASGOW_HASKELL__ " <> op <> " 908")))
              ["if __GLASGOW_HASKELL__ >= 908", "if __GLASGOW_HASKELL__ >= 909"]
        )
        [">=", ">", "<", "<=", "==", "!="]
        `shouldBe` [ [Just True, Nothing],
                     [Just True, Just True],
                     [Just False, Just False],
                     [Nothing, Just False],
                     [Just True, Just False],
                     [Nothing, Nothing]
                   ]

    it "is settled by a failing guard as well" $
      fmap
        (guardHolds (impliedBy False "if __GLASGOW_HASKELL__ >= 908"))
        ["if __GLASGOW_HASKELL__ < 908", "if __GLASGOW_HASKELL__ > 910"]
        `shouldBe` [Just True, Just False]

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

    it "blanks what only another compiler would see" $
      ruledOut
        [ "#if defined(__MHS__)",
          "import Data.ZipList",
          "#endif"
        ]
        `shouldBe` ["", "", ""]

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
