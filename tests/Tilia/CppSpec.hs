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
  describe "branches which deliberately abort preprocessing" $ do
    it "formats the valid branches and preserves the error directive" $
      case formatCpp errorAlternative of
        Left why -> expectationFailure (T.unpack why)
        Right formatted -> formatted `shouldSatisfy` T.isInfixOf "#error Unsupported word size"
    it "checks only valid configurations and is idempotent" $ do
      roundTrip errorAlternative `shouldBe` Right ()
      settles errorAlternative `shouldBe` Right ()
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

  describe "every configuration of the output"
    $ it "is the same program as that configuration of the input"
    $ mapM_ (`shouldBe` Right ()) (fmap roundTrip everyFixture)

  describe "formatting an already formatted module"
    $ it "changes nothing, for every module the prototype handles"
    $ mapM_ (`shouldBe` Right ()) (fmap settles everyFixture)

  describe "an #elif chain" $ do
    it "is printed back as one conditional rather than as nested ones" $
      formatCpp withElif
        `shouldBe` Right
          ( T.unlines
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
          )

    it "keeps its shape when the chain has no #else" $
      formatCpp elifWithoutElse
        `shouldBe` Right
          ( T.unlines
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
          )

  describe "conditionals nested inside one another" $ do
    it "are printed back nested, not flattened into compound conditions" $
      formatCpp nested
        `shouldBe` Right
          ( T.unlines
              [ "module M where",
                "",
                "#if OUTER",
                "a = 1",
                "",
                "#if INNER",
                "b = 2",
                "#endif",
                "#else",
                "a = 3",
                "#endif",
                "",
                "after = 4"
              ]
          )

    it "reach three leaf configurations rather than four" $
      said (length <$> leaves nested) `shouldBe` Right 3

  describe "several conditionals side by side" $ do
    it "each end up around what they were written around" $
      formatCpp twoConditionals
        `shouldBe` Right
          ( T.unlines
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
          )

    it "multiply, as configurations" $
      said (length <$> leaves twoConditionals) `shouldBe` Right 4

    it "add, as formattings: twenty of them are twenty-one, not a million" $
      formatCpp (sideBySide 20) `shouldSatisfy` isRight

    it "are refused once even the sum is more than the budget allows" $
      formatCpp (sideBySide 100) `shouldSatisfy` isLeft

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

  describe "conditionals that reach into the same construct" $ do
    it "are still formatted, by varying them together" $
      roundTrip twoInOneExpression `shouldBe` Right ()

    it "come back nested, still covering all four configurations" $
      (formatCpp twoInOneExpression >>= said . leaves) `shouldSatisfy` either (const False) ((== 4) . length)

    it "still settle" $
      settles twoInOneExpression `shouldBe` Right ()

  describe "a conditional the construct around it straddles" $ do
    it "comes to rest on the context, with nothing written out twice" $
      formatCpp conditionalContext
        `shouldBe` Right
          ( T.unlines
              [ "module M where",
                "",
                "f ::",
                "#ifdef A",
                "  (Ord a) =>",
                "#endif",
                "  a -> [(String, Int)] -> Maybe String -> Either String Int -> IO ()",
                "f x pairs fallback outcome = print (x, pairs, fallback, outcome)"
              ]
          )

    it "gives back what was written" $
      formatCpp conditionalContext `shouldBe` Right conditionalContext

    it "settles on the first pass" $
      settles conditionalContext `shouldBe` Right ()

    it "reads back as the same program in every configuration" $
      roundTrip conditionalContext `shouldBe` Right ()

  describe "a conditional inside an expression" $ do
    it "is left exactly where it was written" $
      formatCpp splitExpression
        `shouldBe` Right "module M where\n\nf x =\n  g x\n#ifdef FOO\n    + 1\n#endif\n"

    it "still reads back as the same program in every configuration" $
      roundTrip splitExpression `shouldBe` Right ()

  describe "a conditional whose branches say the same thing"
    $ it "is kept, because the branches are not at the same spans"
    $ formatCpp sameEitherWay
      `shouldBe` Right "module M where\n\n#ifdef FOO\nmid = 2\n#else\nmid = 2\n#endif\n"

  describe "a directive that asks nothing" $ do
    it "comes back at the line it was written on" $
      formatCpp withDefine
        `shouldBe` Right "module M where\n\n#define N 1\nf = N\n"

    it "settles" $
      settles withDefine `shouldBe` Right ()

    it "still reads back as the same program" $
      roundTrip withDefine `shouldBe` Right ()

    it "is refused when the module is not Haskell without expanding it" $
      formatCpp macroDeclaration `shouldSatisfy` isLeft

    xit "does not run a Haddock into the comment under it" $
      roundTrip defineBetweenConditionals `shouldBe` Right ()

  describe "directives the prototype cannot read" $ do
    it "refuses a module whose conditionals do not balance" $
      formatCpp unbalanced `shouldSatisfy` isLeft

    it "refuses an #else that comes before an #elif" $
      formatCpp elseBeforeElif `shouldSatisfy` isLeft

    it "refuses a conditional no configuration can be parsed out of" $
      formatCpp unparseableAlone `shouldSatisfy` isLeft

----------------------------------------------------------------------------
-- The modules the question is asked of

errorAlternative :: Text
errorAlternative =
  T.unlines
    [ "{-# LANGUAGE CPP #-}",
      "module M where",
      "value :: Int",
      "value = if True then",
      "#if WORD_SIZE_IN_BITS == 64",
      "  64",
      "#elif WORD_SIZE_IN_BITS == 32",
      "  32",
      "#else",
      "#error Unsupported word size",
      "#endif",
      "  else 0"
    ]

-- | Everything that is meant to come out the other side, for the properties
-- that should hold of all of it.
everyFixture :: [Text]
everyFixture =
  [ atDeclarations,
    unevenBranches,
    withComments,
    commentInsideBranch,
    packedTogether,
    differingImports,
    withoutElse,
    withElif,
    elifWithoutElse,
    nested,
    twoConditionals,
    twoInOneExpression,
    splitExpression
  ]

-- | A directive that asks nothing, between two conditionals that ask the
-- same question.
--
-- The merge has no answer for this and wraps the module in a conditional
-- rather than the conditionals in the module. See the held-back example that
-- names it.
defineBetweenConditionals :: Text
defineBetweenConditionals =
  T.unlines
    [ "module M where",
      "",
      "-- | documentation",
      "#if FLAG",
      "-- a remark",
      "f9 = 9",
      "#endif",
      "#define WIDE 1",
      "#if FLAG",
      "-- a remark",
      "#endif"
    ]

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

-- | Branches that import different modules, which is the commonest thing a
-- real conditional does.
differingImports :: Text
differingImports =
  T.unlines
    [ "module M where",
      "",
      "#ifdef FOO",
      "import Data.Map",
      "#else",
      "import Data.Set",
      "#endif",
      "",
      "f = 1"
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

-- | A conditional around a signature's context, which the signature straddles.
--
-- The branches are of unequal length, so the construct the conditional is
-- inside occupies different lines in the two configurations — which is the
-- one case where a span honestly differs without anything having gone wrong.
-- The type is written long enough not to fit on a line once the context is
-- there and to fit comfortably once it is not, so the two configurations also
-- disagree about how to lay the group out.
conditionalContext :: Text
conditionalContext =
  T.unlines
    [ "module M where",
      "",
      "f ::",
      "#ifdef A",
      "  (Ord a) =>",
      "#endif",
      "  a -> [(String, Int)] -> Maybe String -> Either String Int -> IO ()",
      "f x pairs fallback outcome = print (x, pairs, fallback, outcome)"
    ]

-- | A conditional in the middle of an expression, which every configuration
-- can be parsed out of, but which no span survives.
splitExpression :: Text
splitExpression =
  T.unlines
    [ "module M where",
      "",
      "f x =",
      "  g x",
      "#ifdef FOO",
      "    + 1",
      "#endif"
    ]

-- | Two conditionals reaching into one expression, so that their
-- differences land in the same place and cannot be applied side by side.
twoInOneExpression :: Text
twoInOneExpression =
  T.unlines
    [ "module M where",
      "",
      "f =",
      "  a",
      "#if X",
      "    + b",
      "#endif",
      "#if Y",
      "    + c",
      "#endif"
    ]

-- | A conditional whose two branches say the same thing.
sameEitherWay :: Text
sameEitherWay =
  T.unlines
    [ "module M where",
      "",
      "#ifdef FOO",
      "mid = 2",
      "#else",
      "mid  =  2",
      "#endif"
    ]

-- | A conditional that opens a bracket it does not close, so that dropping
-- the branch leaves something that is not a Haskell module at all.
--
-- Harder to come by than it looks. A conditional holding the only statement
-- of a @do@ block, or the only alternative of a @case@, still leaves both
-- configurations parsing, since GHC2021 has @EmptyCase@ and takes an empty
-- @do@. It is unbalanced delimiters that no blanking can rescue.
unparseableAlone :: Text
unparseableAlone =
  T.unlines
    [ "module M where",
      "",
      "#ifdef FOO",
      "f = (1",
      "#endif",
      "  + 2)"
    ]

-- | An @#if@ with nothing to close it.
unbalanced :: Text
unbalanced = T.unlines ["module M where", "", "#ifdef FOO", "f = 1"]

-- | A directive that is not a conditional, and so cannot be blanked away.
withDefine :: Text
withDefine = T.unlines ["module M where", "", "#define N 1", "f = N"]

-- | A macro standing for a piece of syntax rather than for a piece of
-- program.
--
-- @CLOSE@ is a bracket, so the module is only balanced once the macro has
-- been expanded. Nothing short of expanding it makes this Haskell, no
-- configuration of it parses, and there is no document to build. The same
-- shape as a conditional that opens a bracket it does not close, and refused
-- for the same reason.
macroDeclaration :: Text
macroDeclaration =
  T.unlines
    [ "module M where",
      "",
      "#define CLOSE )",
      "f = (1 CLOSE"
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

-- | Format a module, then check that every configuration of what came out is
-- the same program as that configuration of what went in.
--
-- The variational form of the check the corpus already makes of every
-- example, with a @forall cfg@ in front of it. The conditionals come back in
-- the order they were written, so the two enumerations line up leaf for leaf
-- — and if they did not, the count would say so first.
roundTrip :: Text -> Either Text ()
roundTrip source = do
  formatted <- formatCpp source
  pairs <- said (correspondingBranches source formatted)
  mapM_ sameBranch pairs

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

-- | Whether formatting what was formatted changes anything.
--
-- The property the corpus makes of every ordinary example. It is worth
-- asking separately here because the merge is the one part of the printer
-- whose input is its own output: directives go into the text, and the second
-- pass has to split on the very ones the first pass wrote.
settles :: Text -> Either Text ()
settles source = do
  once <- formatCpp source
  twice <- formatCpp once
  if once == twice
    then Right ()
    else Left ("did not settle:\n" <> once <> "\nbecame:\n" <> twice)

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
