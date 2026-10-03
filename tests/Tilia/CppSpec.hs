{-# LANGUAGE OverloadedStrings #-}

-- | Whether documents printed from different configurations line up.
module Tilia.CppSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Cpp
import Tilia.Doc.Internal (Doc)
import Tilia.Equivalence (syntaxDifference)
import Tilia.Parser (defaultParserConfig, parseModule, pmModule)
import Tilia.Render (defaultRenderConfig, renderModule)
import Tilia.Span (Span)

-- | Format a module which uses the C preprocessor, configured with nothing.
formatCpp :: Text -> Either Text Text
formatCpp = said . formatWithCpp defaultParserConfig defaultRenderConfig "example.hs"

spec :: Spec
spec = do
  describe "matching configurations before and after formatting" $ do
    it "does not mistake different whitespace for another configuration" $ do
      let input = "module M where\n#if FLAG\nf=1\n#else\nf = 1\n#endif\n"
      (said (correspondingBranches input "module M where\nf = 1\n") >>= mapM_ sameBranch)
        `shouldBe` Right ()
    it "keeps branch identity when sorting the source text would swap it" $ do
      let input = "module M where\n#if FLAG\nf=1\n#else\nf = 2\n#endif\n"
          output = "module M where\n#if FLAG\nf = 1\n#else\nf = 2\n#endif\n"
      (said (correspondingBranches input output) >>= mapM_ sameBranch) `shouldBe` Right ()
    it "detects moving an error to a different branch" $ do
      let input = "module M where\n#if FLAG\n#error no\n#else\nf = 1\n#endif\n"
          output = "module M where\n#if FLAG\nf = 1\n#else\n#error no\n#endif\n"
      (said (correspondingBranches input output) >>= mapM_ sameBranch) `shouldSatisfy` isLeft
  describe "splitting a module on its conditional" $ do
    it "keeps the directive as written, keyword and all" $
      cfgGuards <$> configurations atDeclarations
        `shouldBe` Just (fmap Guard ["ifdef FOO"])

    it "keeps every line where it was" $
      let sameLength c = all ((== length (T.lines atDeclarations)) . length . T.lines) (cfgTexts c)
       in (sameLength <$> configurations atDeclarations) `shouldBe` Just True

    it "has one more configuration than it has directives" $
      let balanced c = length (cfgTexts c) == length (cfgGuards c) + 1
       in fmap (fmap balanced . configurations) [atDeclarations, withoutElse, withElif]
            `shouldBe` [Just True, Just True, Just True]

    it "finds every directive of an #elif chain, in order" $
      cfgGuards <$> configurations withElif
        `shouldBe` Just (fmap Guard ["if A", "elif B"])

    it "takes only the outermost conditional, leaving the nested one alone" $
      cfgGuards <$> configurations nested
        `shouldBe` Just (fmap Guard ["if OUTER"])

    it "takes only the first of two conditionals side by side" $
      cfgGuards <$> configurations twoConditionals
        `shouldBe` Just (fmap Guard ["if FIRST"])

    it "declines a module with no conditional at all" $
      configurations "module M where\nf = 1\n" `shouldBe` Nothing

  describe "the regions the conditional does not touch" $ do
    it "are most of the module, so the comparison is not vacuous" $
      overlap atDeclarations `shouldSatisfy` either (const False) (> 10)

    it "print identically in every configuration, at a declaration boundary" $
      disagreements atDeclarations `shouldBe` Right []

    it "print identically when the branches are of different lengths" $
      disagreements unevenBranches `shouldBe` Right []

    it "print identically when a comment sits next to the conditional" $
      disagreements withComments `shouldBe` Right []

    it "print identically when a comment lives inside one branch" $
      disagreements commentInsideBranch `shouldBe` Right []

    it "print identically with nothing to separate the conditional" $
      disagreements packedTogether `shouldBe` Right []

    it "print identically when the conditional has no alternative" $
      disagreements withoutElse `shouldBe` Right []

    it "print identically across all three branches of an #elif" $
      disagreements withElif `shouldBe` Right []

  describe "conditionals nested inside one another"
    $ it "reach three leaf configurations rather than four"
    $ said (length <$> leaves nested) `shouldBe` Right 3

  describe "several conditionals side by side" $ do
    it "multiply, as configurations" $
      said (length <$> leaves twoConditionals) `shouldBe` Right 4

    it "add, as formattings: twenty of them are twenty-one, not a million" $
      formatCpp (sideBySide 20) `shouldSatisfy` isRight

    it "cost only the declarations each of them reaches" $
      formatCpp (apart 400) `shouldSatisfy` isRight

    it "are refused once even the sum is more than the budget allows" $
      formatCpp (oneDeclaration 100 1000) `shouldSatisfy` isLeft

  describe "counting the configurations" $ do
    it "agrees with enumerating them, where enumerating them is possible" $
      let counted m = (said (countLeaves m), said (toInteger . length <$> leaves m))
       in fmap
            counted
            [ atDeclarations,
              withElif,
              elifWithoutElse,
              withoutElse,
              nested,
              twoConditionals,
              sideBySide 8
            ]
            `shouldSatisfy` all (uncurry (==))

    it "does not enumerate them, where enumerating them is not" $
      said (countLeaves (sideBySide 63)) `shouldBe` Right (2 ^ (63 :: Int))

    it "counts two conditionals behind one guard as one conditional" $
      said (countLeaves (sameGuard 20)) `shouldBe` Right 2

  describe "one guard asked at two depths" $ do
    it "is one question, however deep the second asking is" $
      said (countLeaves guardAtTwoDepths) `shouldBe` Right 4

    it "counts what enumerating them produces" $
      (said (countLeaves guardAtTwoDepths), said (toInteger . length <$> leaves guardAtTwoDepths))
        `shouldSatisfy` uncurry (==)

    it "is never answered one way at the top and the other way inside" $
      said (leaves guardAtTwoDepths)
        `shouldSatisfy` either
          (const False)
          (all (\l -> not ("inner" `T.isInfixOf` l) || "outer" `T.isInfixOf` l))

  describe "covering every branch" $ do
    it "gives a module with no conditionals one configuration, its own" $
      said (length <$> branchLeaves "module M where\nx = 1\n") `shouldBe` Right 1

    it "gives one per branch of a conditional" $ do
      said (length <$> branchLeaves "module M where\n#if A\nx = 1\n#endif\n") `shouldBe` Right 2
      said (length <$> branchLeaves "module M where\n#if A\nx = 1\n#else\nx = 2\n#endif\n")
        `shouldBe` Right 2

    -- A module written on Windows ends every line with a carriage return,
    -- @#endif@ included, and one that closes no group leaves the whole
    -- module unsplittable. Every module of @crypton-pem@ is written this
    -- way, and refusing them refused everything that reads a certificate.
    it "gives one per branch when the lines end in a carriage return" $
      said (length <$> branchLeaves "module M where\r\n#if A\r\nx = 1\r\n#else\r\nx = 2\r\n#endif\r\n")
        `shouldBe` Right 2

    it "is their sum where enumerating them would be their product" $
      (said (length <$> branchLeaves (sideBySide 20)), said (countLeaves (sideBySide 20)))
        `shouldBe` (Right 21, Right (2 ^ (20 :: Int)))

  describe "varying one conditional at a time" $ do
    it "is the baseline and one configuration per further branch" $
      said (length <$> linearLeaves (sideBySide 63)) `shouldBe` Right 64

    it "agrees with enumerating them where a module has one conditional" $
      (said (linearLeaves atDeclarations), said (leaves atDeclarations))
        `shouldSatisfy` uncurry (==)

  describe "directives the prototype cannot read" $ do
    it "refuses a module whose conditionals do not balance" $
      formatCpp unbalanced `shouldBe` Left "the #ifdef at line 3 is never closed"

    it "refuses an #endif with no conditional to close" $
      formatCpp strayEndif `shouldBe` Left "the #endif at line 4 has no conditional to belong to"

    it "refuses an #else that comes before an #elif" $
      formatCpp elseBeforeElif
        `shouldBe` Left "the #elif at line 7 comes after the #else of its conditional"

    it "refuses a directive inside a quasiquote" $
      formatCpp includeInQuasiquote
        `shouldBe` Left "the #include at line 7 is inside a quasi-quote or a multi-line string"

    it "refuses an alternative aborting with #error that it cannot format" $
      formatCpp abortingAlternative
        `shouldBe` Left "the alternative that the #error at line 7 aborts cannot be formatted without parsing it"

    it "refuses a branch that a conditional around it asking the same question rules out" $
      formatCpp ruledOut
        `shouldBe` Left "the branch at line 6 is ruled out by a conditional around it that asks the same question"

----------------------------------------------------------------------------
-- The modules the question is asked of

-- | A conditional between two whole declarations, which is the case the
-- design is meant to handle.
atDeclarations :: Text
atDeclarations =
  T.unlines
    [ "module M where",
      "",
      "before :: Int",
      "before = 1",
      "",
      "#ifdef FOO",
      "mid :: Int",
      "mid = 2",
      "#else",
      "mid :: Int",
      "mid = 3",
      "#endif",
      "",
      "after :: Int",
      "after = 4"
    ]

-- | Branches that print to different numbers of lines, so that anything
-- downstream of them would shift if positions were being followed.
unevenBranches :: Text
unevenBranches =
  T.unlines
    [ "module M where",
      "",
      "before = 1",
      "",
      "#ifdef FOO",
      "mid = case x of",
      "  A -> 1",
      "  B -> 2",
      "#else",
      "mid = 3",
      "#endif",
      "",
      "after = 4"
    ]

-- | A comment inside one branch and not the other. Comment placement reads
-- every region in the document, so the declarations outside the conditional
-- are being asked about under two different comment streams.
commentInsideBranch :: Text
commentInsideBranch =
  T.unlines
    [ "module M where",
      "",
      "before = 1",
      "",
      "#ifdef FOO",
      "-- a note only this branch has",
      "mid = 2 -- and a trailing one",
      "#else",
      "mid = 3",
      "#endif",
      "",
      "after = 4"
    ]

-- | No blank lines anywhere, so that whether one is printed between
-- declarations is decided by what lies between their spans.
packedTogether :: Text
packedTogether =
  T.unlines
    [ "module M where",
      "before = 1",
      "#ifdef FOO",
      "mid = 2",
      "#else",
      "mid = 3",
      "#endif",
      "after = 4"
    ]

-- | A comment on either side of the conditional. Comment attachment reads
-- the whole document, so this is where context-sensitivity would show.
withComments :: Text
withComments =
  T.unlines
    [ "module M where",
      "",
      "-- above",
      "before = 1 -- trailing",
      "",
      "#ifdef FOO",
      "mid = 2",
      "#else",
      "mid = 3",
      "#endif",
      "",
      "-- below",
      "after = 4"
    ]

-- | A conditional with no alternative, which is what most conditionals in
-- real Haskell source are: a @MIN_VERSION@ test around a definition that
-- newer or older compilers do not want.
withoutElse :: Text
withoutElse =
  T.unlines
    [ "module M where",
      "",
      "before = 1",
      "",
      "#if MIN_VERSION_base(4,19,0)",
      "mid = 2",
      "#endif",
      "",
      "after = 4"
    ]

-- | Three branches, so that the choice is genuinely n-ary and not a pair
-- with extra steps.
withElif :: Text
withElif =
  T.unlines
    [ "module M where",
      "",
      "before = 1",
      "",
      "#if A",
      "mid = 1",
      "#elif B",
      "mid = 2",
      "#else",
      "mid = 3",
      "#endif",
      "",
      "after = 4"
    ]

-- | An @#elif@ chain that stops without an @#else@, so that the last
-- configuration is the one where nothing at all is taken.
elifWithoutElse :: Text
elifWithoutElse =
  T.unlines
    [ "module M where",
      "",
      "#if A",
      "mid = 1",
      "#elif B",
      "mid = 2",
      "#endif",
      "",
      "after = 4"
    ]

-- | A conditional inside a branch of another, which the splitter must leave
-- to the recursion rather than read as a chain.
nested :: Text
nested =
  T.unlines
    [ "module M where",
      "",
      "#if OUTER",
      "a = 1",
      "#if INNER",
      "b = 2",
      "#endif",
      "#else",
      "a = 3",
      "#endif",
      "",
      "after = 4"
    ]

-- | Two conditionals with a declaration between them, neither inside the
-- other.
twoConditionals :: Text
twoConditionals =
  T.unlines
    [ "module M where",
      "",
      "#if FIRST",
      "a = 1",
      "#else",
      "a = 2",
      "#endif",
      "",
      "between = 0",
      "",
      "#if SECOND",
      "b = 1",
      "#endif",
      "",
      "after = 4"
    ]

-- | A module with @n@ conditionals in a row, for asking where the budget
-- stops.
sideBySide :: Int -> Text
sideBySide n =
  T.unlines $
    ["module M where", ""]
      <> concat
        [ [ "#if C" <> T.pack (show i),
            "x" <> T.pack (show i) <> " = 1",
            "#else",
            "x" <> T.pack (show i) <> " = 2",
            "#endif"
          ]
        | i <- [1 .. n]
        ]

-- | A module with @n@ conditionals, each around a declaration of its own
-- and kept apart from the next by one that is not.
apart :: Int -> Text
apart n =
  T.unlines $
    ["module M where", ""]
      <> concat
        [ [ "#if C" <> T.pack (show i),
            "x" <> T.pack (show i) <> " = 1",
            "#else",
            "x" <> T.pack (show i) <> " = 2",
            "#endif",
            "",
            "y" <> T.pack (show i) <> " = 0",
            ""
          ]
        | i <- [1 .. n]
        ]

-- | A module with one declaration holding @n@ conditionals and @m@ lines
-- that are in every configuration.
oneDeclaration :: Int -> Int -> Text
oneDeclaration n m =
  T.unlines $
    ["module M where", "", "x =", "  [ 0"]
      <> concat
        [ ["#if C" <> T.pack (show i), "  , 1", "#endif"]
        | i <- [1 .. n]
        ]
      <> replicate m "  , 0"
      <> ["  ]"]

-- | A module with @n@ conditionals in a row, all asking the same question.
--
-- Which makes them one question, however many times it is written down. The
-- shape a module reaches by being formatted, since aligning the alternatives
-- can leave one conditional printed as several.
sameGuard :: Int -> Text
sameGuard n =
  T.unlines $
    ["module M where", ""]
      <> concat
        [ [ "#if C",
            "x" <> T.pack (show i) <> " = 1",
            "#else",
            "x" <> T.pack (show i) <> " = 2",
            "#endif"
          ]
        | i <- [1 .. n]
        ]

-- | One guard asked twice, once at the top level and once inside another
-- conditional's branch.
--
-- Untied this has six configurations where it has four, and one of the two
-- extra ones answers @A@ both ways at once.
guardAtTwoDepths :: Text
guardAtTwoDepths =
  T.unlines
    [ "module M where",
      "",
      "#if A",
      "outer = 1",
      "#endif",
      "",
      "#ifdef B",
      "beside = 2",
      "#if A",
      "inner = 3",
      "#endif",
      "#endif"
    ]

-- | An @#if@ with nothing to close it.
unbalanced :: Text
unbalanced = T.unlines ["module M where", "", "#ifdef FOO", "f = 1"]

-- | An @#endif@ with nothing to close.
strayEndif :: Text
strayEndif = T.unlines ["module M where", "", "f = 1", "#endif"]

-- | A directive that the preprocessor acts on although it is written inside
-- a quasiquote, which is reproduced verbatim.
includeInQuasiquote :: Text
includeInQuasiquote =
  T.unlines
    [ "{-# LANGUAGE QuasiQuotes #-}",
      "",
      "module M where",
      "",
      "banner =",
      "  [template|",
      "#include \"banner.txt\"",
      "|]"
    ]

-- | An alternative that aborts with @#error@ and leaves an expression
-- unfinished, in a module whose conditionals cannot be varied one at a
-- time.
abortingAlternative :: Text
abortingAlternative =
  T.unlines
    [ "module M where",
      "",
      "f =",
      "#if A",
      "  1",
      "#else",
      "#error \"b\"",
      "#endif",
      "  + 2",
      "#if A",
      "g = 1",
      "#endif"
    ]

-- | An @#else@ that the @#if@ around it rules out, which no configuration
-- takes.
ruledOut :: Text
ruledOut =
  T.unlines
    [ "module M where",
      "",
      "#if FLAG",
      "#if FLAG",
      "f = 1",
      "#else",
      "f = 2",
      "#endif",
      "#endif"
    ]

-- | An @#else@ with an @#elif@ after it, which no preprocessor would accept
-- and which the splitter must not quietly reorder into something it would.
elseBeforeElif :: Text
elseBeforeElif =
  T.unlines
    [ "module M where",
      "",
      "#if A",
      "f = 1",
      "#else",
      "f = 2",
      "#elif B",
      "f = 3",
      "#endif"
    ]

----------------------------------------------------------------------------
-- Asking it

isLeft, isRight :: Either a b -> Bool
isLeft = either (const True) (const False)
isRight = either (const False) (const True)

sameBranch :: (Maybe Text, Maybe Text) -> Either Text ()
sameBranch (Nothing, Nothing) = Right ()
sameBranch (Just input, Just output) = sameProgram input output
  where
    sameProgram before' after' = do
      a <- moduleOf before'
      b <- moduleOf after'
      case syntaxDifference a b of
        Nothing -> Right ()
        Just difference -> Left ("a different program: " <> difference)
    moduleOf text = case parseModule defaultParserConfig "<cpp>" text of
      Left _ -> Left ("did not parse:\n" <> text)
      Right parsed -> Right (pmModule parsed)
sameBranch _ = Left "preprocessing changed between succeeding and failing"

-- | A refusal as words, which is the only place these tests want one.
said :: Either CppError a -> Either Text a
said = either (Left . describeCppError) Right

-- | The spans every configuration printed, but did not all print alike.
--
-- 'Left' when a configuration did not parse or the module had no
-- conditional, so that a broken fixture is not mistaken for agreement.
disagreements :: Text -> Either String [Span]
disagreements = fmap (Map.keys . Map.filter not) . agreement

-- | How many spans every configuration printed.
overlap :: Text -> Either String Int
overlap = fmap Map.size . agreement

-- | The spans every configuration of a module's first conditional printed,
-- and whether they all printed them alike.
agreement :: Text -> Either String (Map.Map Span Bool)
agreement source = do
  c <- maybe (Left "no conditional") Right (configurations source)
  docs <- traverse documentOf (cfgTexts c)
  case fmap regions docs of
    [] -> Left "a conditional with no branches"
    (first' : rest) -> Right (foldl' (narrow first') (True <$ first') rest)
  where
    narrow first' acc other =
      Map.intersectionWith (&&) acc (Map.intersectionWith (==) first' other)

documentOf :: Text -> Either String Doc
documentOf source = case parseModule defaultParserConfig "<cpp>" source of
  Left _ -> Left ("did not parse:\n" <> T.unpack source)
  Right parsed -> Right (renderModule defaultRenderConfig parsed)
