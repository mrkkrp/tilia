{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Inferences regarding CPP macros that we can make based on the build
-- plan and on the answers a configuration gives.
module Tilia.Cpp.Macros
  ( Macros (..),
    noMacros,
    guardHolds,
    definedness,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | What is known about macros.
data Macros = Macros
  { -- | The macros that take a version apart and compare it:
    -- @MIN_VERSION_containers@ and the like, each under the version the
    -- plan resolved that package to. @MIN_VERSION_GLASGOW_HASKELL@ is one
    -- of these, under the compiler's four-part version.
    macroVersions :: Map Text [Integer],
    -- | The macros that stand for a number, which is @__GLASGOW_HASKELL__@
    -- and its patch levels.
    macroNumbers :: Map Text Integer,
    -- | The macros known to be defined, though not to what.
    macroDefined :: Set Text,
    -- | The macros known not to be defined, such as those of the compilers
    -- other than the one the plan is for, like @__MHS__@.
    macroUndefined :: Set Text
  }
  deriving (Eq, Show)

-- | Nothing known about any macro.
noMacros :: Macros
noMacros =
  Macros
    { macroVersions = Map.empty,
      macroNumbers = Map.empty,
      macroDefined = Set.empty,
      macroUndefined = Set.empty
    }

-- | Does a conditional's guard hold, where what is known settles it?
-- 'Nothing' is no answer rather than a negative one.
guardHolds :: Macros -> Text -> Maybe Bool
guardHolds macros written = case T.span isNameChar (T.stripStart written) of
  (keyword, rest) -> case keyword of
    "if" -> (/= 0) <$> valueOf macros rest
    "elif" -> (/= 0) <$> valueOf macros rest
    "ifdef" -> nameIsDefined rest
    "elifdef" -> nameIsDefined rest
    "ifndef" -> not <$> nameIsDefined rest
    "elifndef" -> not <$> nameIsDefined rest
    _ -> Nothing
  where
    nameIsDefined rest = case tokensOf rest of
      Just [Name n] -> isDefined macros n
      _ -> Nothing

-- | Whether a macro is defined, where that is known.
isDefined :: Macros -> Text -> Maybe Bool
isDefined macros n
  | Map.member n (macroVersions macros)
      || Map.member n (macroNumbers macros)
      || Set.member n (macroDefined macros) =
      Just True
  | Set.member n (macroUndefined macros) = Just False
  | otherwise = Nothing

-- | What a guard holding, or failing, tells about which macros are defined:
-- each macro it settles, and whether it is defined.
--
-- Only a guard that asks about one macro alone settles anything.
definedness :: Bool -> Text -> [(Text, Bool)]
definedness holds written = case T.span isNameChar (T.stripStart written) of
  (keyword, rest) -> case tokensOf rest of
    Just [Name n]
      | keyword `elem` ["ifdef", "elifdef"] -> [(n, holds)]
      | keyword `elem` ["ifndef", "elifndef"] -> [(n, not holds)]
    Just ts | keyword `elem` ["if", "elif"] -> asked holds ts
    _ -> []
  where
    asked h = \case
      Punct "!" : ts -> asked (not h) ts
      [Name "defined", Name n] -> [(n, h)]
      [Name "defined", Punct "(", Name n, Punct ")"] -> [(n, h)]
      [Name n] | h -> [(n, True)]
      _ -> []

-- | What the expression of an @#if@ or @#elif@ evaluates to, if anything.
valueOf :: Macros -> Text -> Maybe Integer
valueOf macros written = case tokensOf written of
  Nothing -> Nothing
  Just ts -> case orExpr macros ts of
    Just (value, []) -> value
    _ -> Nothing

-- | One piece of a guard.
data Token
  = Name Text
  | Number Integer
  | Punct Text
  deriving (Eq, Show)

-- | Take a guard apart, or refuse it whole.
tokensOf :: Text -> Maybe [Token]
tokensOf = go . T.stripStart
  where
    go t
      | T.null t = Just []
      | Just (c, _) <- T.uncons t,
        isNameStart c =
          let (n, rest) = T.span isNameChar t in (Name n :) <$> next rest
      | Just (c, _) <- T.uncons t,
        isDigit c =
          let (digits, rest) = T.span isDigit t
              rest' = T.dropWhile (`T.elem` "uUlL") rest
           in case T.uncons rest' of
                Just (c', _) | isNameChar c' || c' == '.' -> Nothing
                _ -> (Number (readDigits digits) :) <$> next rest'
      | Just punct <- firstThat (`T.stripPrefix` t) punctuation =
          (Punct (T.take (T.length t - T.length punct) t) :) <$> next punct
      | otherwise = Nothing
    next = go . T.stripStart
    firstThat f = foldr (\x acc -> maybe acc Just (f x)) Nothing
    readDigits = T.foldl' (\n c -> n * 10 + toInteger (fromEnum c - fromEnum '0')) 0

-- | An expression, and what is left of the tokens after it.
--
-- The outer 'Maybe' is whether it could be read at all; the inner one is
-- whether what it means is known.
type Reading = Maybe (Maybe Integer, [Token])

orExpr :: Macros -> [Token] -> Reading
orExpr macros ts = do
  (left, rest) <- andExpr macros ts
  more left rest
  where
    more left = \case
      Punct "||" : rest -> do
        (right, rest') <- andExpr macros rest
        more (either' left right) rest'
      rest -> Just (left, rest)
    either' a b
      | any true [a, b] = Just 1
      | all false [a, b] = Just 0
      | otherwise = Nothing

andExpr :: Macros -> [Token] -> Reading
andExpr macros ts = do
  (left, rest) <- compared macros ts
  more left rest
  where
    more left = \case
      Punct "&&" : rest -> do
        (right, rest') <- compared macros rest
        more (both left right) rest'
      rest -> Just (left, rest)
    both a b
      | any false [a, b] = Just 0
      | all true [a, b] = Just 1
      | otherwise = Nothing

true, false :: Maybe Integer -> Bool
true = maybe False (/= 0)
false = maybe False (== 0)

compared :: Macros -> [Token] -> Reading
compared macros ts = do
  (left, rest) <- unary macros ts
  case rest of
    Punct op : rest' | Just test <- comparison op -> do
      (right, rest'') <- unary macros rest'
      pure (fromBool . uncurry test <$> pair left right, rest'')
    _ -> Just (left, rest)
  where
    pair a b = (,) <$> a <*> b
    fromBool b = if b then 1 else 0
    comparison = \case
      "==" -> Just (==)
      "!=" -> Just (/=)
      "<" -> Just (<)
      ">" -> Just (>)
      "<=" -> Just (<=)
      ">=" -> Just (>=)
      _ -> Nothing

unary :: Macros -> [Token] -> Reading
unary macros = \case
  Punct "!" : rest -> do
    (value, rest') <- unary macros rest
    pure (negated <$> value, rest')
  Punct "(" : rest -> do
    (value, rest') <- orExpr macros rest
    case rest' of
      Punct ")" : rest'' -> Just (value, rest'')
      _ -> Nothing
  Name "defined" : rest -> case rest of
    Name n : rest' -> Just (asDefined n, rest')
    Punct "(" : Name n : Punct ")" : rest' -> Just (asDefined n, rest')
    _ -> Nothing
  Name n : Punct "(" : rest -> do
    (arguments, rest') <- argumentList rest
    pure (atLeast <$> Map.lookup n (macroVersions macros) <*> arguments, rest')
  Name n : rest -> Just (number n, rest)
  Number n : rest -> Just (Just n, rest)
  _ -> Nothing
  where
    negated n = if n == 0 then 1 else 0
    asDefined n = (\d -> if d then 1 else 0) <$> isDefined macros n
    number n = case isDefined macros n of
      Just False -> Just 0
      _ -> Map.lookup n (macroNumbers macros)
    atLeast held wanted = if pad held >= pad wanted then 1 else 0
      where
        width = max (length held) (length wanted)
        pad v = take width (v <> repeat 0)

argumentList :: [Token] -> Maybe (Maybe [Integer], [Token])
argumentList ts = do
  (inside, rest) <- upToClose (0 :: Int) [] ts
  pure (numbersOf inside, rest)
  where
    upToClose depth acc = \case
      Punct ")" : rest
        | depth == 0 -> Just (reverse acc, rest)
        | otherwise -> upToClose (depth - 1) (Punct ")" : acc) rest
      Punct "(" : rest -> upToClose (depth + 1) (Punct "(" : acc) rest
      t : rest -> upToClose depth (t : acc) rest
      [] -> Nothing
    numbersOf = \case
      [Number n] -> Just [n]
      Number n : Punct "," : rest -> (n :) <$> numbersOf rest
      _ -> Nothing

-- | The punctuation of the expressions we read, longest first so that @<=@
-- is never taken for @<@.
punctuation :: [Text]
punctuation = ["&&", "||", "==", "!=", "<=", ">=", "(", ")", ",", "!", "<", ">"]

isNameStart :: Char -> Bool
isNameStart c = isAsciiLower c || isAsciiUpper c || c == '_'

isNameChar :: Char -> Bool
isNameChar c = isNameStart c || isDigit c
