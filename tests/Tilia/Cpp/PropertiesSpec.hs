{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Properties that should hold of every module the preprocessor is
-- involved in.
module Tilia.Cpp.PropertiesSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec hiding (after, before)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
import Tilia.Cpp (answeredLeaves, formatWithCpp, withoutRuledOut)
import Tilia.Cpp.Macros (Macros (..))
import Tilia.Equivalence (syntaxDifference)
import Tilia.Parser (defaultParserConfig, parseModule, pmModule)
import Tilia.Render (defaultRenderConfig)
import Tilia.Utils (tshow)

spec :: Spec
spec = modifyMaxSuccess (const 5000) $
  describe "a module the preprocessor runs over" $ do
    xit "reaches its answer in one pass" $
      property $ \m -> formatted m $ \out ->
        case format out of
          Left why -> counterexample (T.unpack ("re-formatting refused: " <> why)) False
          Right settled ->
            counterexample (T.unpack (diffed out settled)) (settled == out)

    it "comes out parseable in every configuration" $
      property $ \m -> formatted m $ \out ->
        case answeredLeaves out of
          Left _ -> property Discard
          Right configurations ->
            conjoin
              [ counterexample (T.unpack ("this configuration does not parse:\n" <> t)) (parses t)
              | (_, t) <- configurations
              ]

    it "is read as configurations it already had, once a plan rules some out" $
      property $ \m ->
        let source = sourceOf m
         in case (answeredLeaves source, answeredLeaves (withoutRuledOut macros source)) of
              (Right went, Right came) ->
                let had = Set.fromList (fmap snd went)
                 in conjoin $
                      -- Selecting a branch with #error may intentionally
                      -- leave no compilable configuration.
                      counterexample
                        "nothing was left to read"
                        (not (null came) || "#error" `T.isInfixOf` source)
                        : [ counterexample
                              (T.unpack ("not a configuration the module had:\n" <> t))
                              (Set.member t had)
                          | (_, t) <- came
                          ]
              _ -> property Discard

    xit "is the same program in every configuration it went in as" $
      property $ \m -> formatted m $ \out ->
        case (answeredLeaves (sourceOf m), answeredLeaves out) of
          (Right went, Right came) ->
            conjoin
              [ counterexample (T.unpack (T.unlines [before, "became", after, why])) False
              | (answers, before) <- went,
                Just after <- [lookup answers came],
                Just why <- [difference before after]
              ]
          _ -> property Discard

----------------------------------------------------------------------------
-- Running the formatter

-- | A plan that settles the version the generator asks about and nothing
-- else, so that a module comes out with some of its conditionals answered
-- and some of them left open.
macros :: Macros
macros =
  Macros
    { macroVersions = Map.fromList [("MIN_VERSION_thing", [1, 2, 3])],
      macroNumbers = Map.empty
    }

format :: Text -> Either Text Text
format source = case formatWithCpp defaultParserConfig defaultRenderConfig "M.hs" source of
  Left _ -> Left "declined"
  Right out -> Right out

-- | Whatever holds of a module the formatter did not decline.
--
-- Declining is an answer the formatter is allowed to give—a @#include@ it
-- cannot expand, a module with more configurations than its budget—and says
-- nothing about the properties below, so those runs are thrown away rather
-- than counted as passes.
formatted :: (Testable p) => CppModule -> (Text -> p) -> Property
formatted m k = case format (sourceOf m) of
  Left _ -> property Discard
  Right out -> property (k out)

parses :: Text -> Bool
parses t = case parseModule defaultParserConfig "M.hs" t of
  Left _ -> False
  Right _ -> True

difference :: Text -> Text -> Maybe Text
difference before after = do
  b <- either (const Nothing) Just (parseModule defaultParserConfig "M.hs" before)
  a <- either (const Nothing) Just (parseModule defaultParserConfig "M.hs" after)
  syntaxDifference (pmModule b) (pmModule a)

diffed :: Text -> Text -> Text
diffed a b = T.unlines ["first pass:", a, "second pass:", b]

----------------------------------------------------------------------------
-- Generating a module

-- | One line, or one run of lines, of a generated module.
data Item
  = -- | A declaration, named after the number so that a counterexample can
    -- be read.
    Decl Int
  | -- | A comment written on a line of its own.
    Note Text
  | -- | An empty line, which is the point of half of these properties.
    Blank
  | -- | A directive that introduces no configuration of its own—@#define@
    -- and the rest of what "Tilia.Cpp" calls opaque.
    Opaque Text
  | -- | A conditional, and what it holds either side of the @#else@.
    Cond Text [Item] [Item]
  deriving (Eq, Show)

-- | A module that uses the preprocessor.
--
-- Conditionals wrap whole items and nothing smaller, which is what keeps
-- every configuration of the module a module: a branch that took half a
-- declaration away would leave the other half behind.
newtype CppModule = CppModule [Item]
  deriving (Eq)

-- | Shown as the source, since that is what a counterexample is read as.
instance Show CppModule where
  show m = T.unpack ("\n" <> sourceOf m)

instance Arbitrary CppModule where
  arbitrary = CppModule <$> sized (items 2)
  shrink (CppModule xs) = CppModule <$> smaller xs

-- | The items of a module, given how deep a conditional may still nest.
items :: Int -> Int -> Gen [Item]
items depth size' = do
  n <- choose (1, max 1 (min 6 size'))
  mapM (const (item depth)) [1 .. n :: Int]

item :: Int -> Gen Item
item depth =
  frequency $
    [ (4, Decl <$> choose (1, 9)),
      (3, Note <$> elements ["-- a remark", "-- | documentation", "{- a block -}", "-- * a heading"]),
      (3, pure Blank),
      (1, Opaque <$> elements ["define WIDE 1", "error \"no\"", "undef WIDE"])
    ]
      <> [(3, conditional depth) | depth > 0]

conditional :: Int -> Gen Item
conditional depth = do
  guard' <-
    elements
      [ "if FLAG",
        "ifdef OTHER",
        "if FLAG",
        "if MIN_VERSION_thing(1,0,0)",
        "if MIN_VERSION_thing(9,0,0)"
      ]
  yes <- items (depth - 1) 3
  no <- frequency [(1, pure []), (1, items (depth - 1) 2)]
  pure (Cond guard' yes no)

-- | Every way of making a module smaller: drop an item, or replace a
-- conditional by one of the branches it was holding.
smaller :: [Item] -> [[Item]]
smaller xs =
  [take i xs <> drop (i + 1) xs | i <- positions]
    <> [take i xs <> branch <> drop (i + 1) xs | (i, Cond _ a b) <- indexed, branch <- [a, b]]
    <> [take i xs <> [x'] <> drop (i + 1) xs | (i, x) <- indexed, x' <- inside x]
  where
    positions = [0 .. length xs - 1]
    indexed = zip positions xs
    inside = \case
      Cond g a b -> [Cond g a' b | a' <- smaller a] <> [Cond g a b' | b' <- smaller b]
      _ -> []

-- | The module as it is written out.
sourceOf :: CppModule -> Text
sourceOf (CppModule xs) =
  T.unlines (["{-# LANGUAGE CPP #-}", "", "module M where", ""] <> concatMap written xs)

written :: Item -> [Text]
written = \case
  Decl n -> ["f" <> tshow n <> " = " <> tshow n]
  Note t -> [t]
  Blank -> [""]
  Opaque t -> ["#" <> t]
  Cond g yes no ->
    ["#" <> g]
      <> concatMap written yes
      <> (if null no then [] else "#else" : concatMap written no)
      <> ["#endif"]
