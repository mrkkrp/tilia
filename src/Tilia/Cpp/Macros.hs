{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Inferences regarding CPP macros that we can make based on the build
-- plan and on the answers a configuration gives.
module Tilia.Cpp.Macros
  ( Macros (..),
    guardHolds,
    impliedBy,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (dropWhileEnd)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
    macroUndefined :: Set Text,
    -- | The version, or number, each macro is known to reach, a number
    -- being a version of one part.
    macroAtLeast :: Map Text [Integer],
    -- | The version, or number, each macro is known to stay below.
    macroBelow :: Map Text [Integer]
  }
  deriving (Eq, Show)

instance Semigroup Macros where
  a <> b =
    Macros
      { macroVersions = macroVersions a <> macroVersions b,
        macroNumbers = macroNumbers a <> macroNumbers b,
        macroDefined = macroDefined a <> macroDefined b,
        macroUndefined = macroUndefined a <> macroUndefined b,
        macroAtLeast = Map.unionWith max (macroAtLeast a) (macroAtLeast b),
        macroBelow = Map.unionWith min (macroBelow a) (macroBelow b)
      }

instance Monoid Macros where
  mempty =
    Macros
      { macroVersions = Map.empty,
        macroNumbers = Map.empty,
        macroDefined = Set.empty,
        macroUndefined = Set.empty,
        macroAtLeast = Map.empty,
        macroBelow = Map.empty
      }

-- | Does a conditional's guard hold, where what is known settles it?
-- 'Nothing' is no answer rather than a negative one.
guardHolds :: Macros -> Text -> Maybe Bool
guardHolds macros written = (/= 0) <$> (valueOf macros =<< expressionOf written)

-- | What a guard holding, or failing, implies about the macros.
impliedBy :: Bool -> Text -> Macros
impliedBy holds = maybe mempty (go holds) . expressionOf
  where
    go h = \case
      Not e -> go (not h) e
      And a b | h -> go h a <> go h b
      Or a b | not h -> go h a <> go h b
      Defined n
        | h -> mempty{macroDefined = Set.singleton n}
        | otherwise -> mempty{macroUndefined = Set.singleton n}
      Reaches n v
        | h -> mempty{macroAtLeast = Map.singleton n (trimmed v)}
        | otherwise -> mempty{macroBelow = Map.singleton n (trimmed v)}
      Macro n | h -> mempty{macroDefined = Set.singleton n}
      _ -> mempty

-- | Whether a macro is defined, where that is known.
isDefined :: Macros -> Text -> Maybe Bool
isDefined macros n
  | Map.member n (macroVersions macros)
      || Map.member n (macroNumbers macros)
      || Set.member n (macroDefined macros) =
      Just True
  | Set.member n (macroUndefined macros) = Just False
  | otherwise = Nothing

-- | The number a macro stands for, where that is known.
numberOf :: Macros -> Text -> Maybe Integer
numberOf macros n = case isDefined macros n of
  Just False -> Just 0
  _ -> Map.lookup n (macroNumbers macros)

-- | Does a macro's version, or number, reach this one, where what is known
-- settles it?
reaches :: Macros -> Text -> [Integer] -> Maybe Bool
reaches macros n wanted = case held of
  Just v -> Just (trimmed v >= w)
  Nothing
    | any (>= w) (Map.lookup n (macroAtLeast macros)) -> Just True
    | any (<= w) (Map.lookup n (macroBelow macros)) -> Just False
    | otherwise -> Nothing
  where
    w = trimmed wanted
    held =
      Map.lookup n (macroVersions macros) <|> (pure <$> numberOf macros n)

-- | A version without the zeros at its end, which 'compare' orders as
-- versions are ordered.
trimmed :: [Integer] -> [Integer]
trimmed = dropWhileEnd (== 0)

-- | The expression of a guard, where we read it.
expressionOf :: Text -> Maybe Expr
expressionOf written = case T.span isNameChar (T.stripStart written) of
  (keyword, rest)
    | keyword `elem` ["if", "elif"] -> case orExpr =<< tokensOf rest of
        Just (e, []) -> Just e
        _ -> Nothing
    | keyword `elem` ["ifdef", "elifdef"] -> Defined <$> nameOf rest
    | keyword `elem` ["ifndef", "elifndef"] -> Not . Defined <$> nameOf rest
    | otherwise -> Nothing
  where
    nameOf rest = case tokensOf rest of
      Just [Name n] -> Just n
      _ -> Nothing

-- | The expression of a guard.
data Expr
  = Or Expr Expr
  | And Expr Expr
  | Not Expr
  | -- | Two expressions and the test comparing them.
    Compared (Integer -> Integer -> Bool) Expr Expr
  | -- | Whether a macro is defined.
    Defined Text
  | -- | Whether a macro's version, or number, reaches this one.
    Reaches Text [Integer]
  | -- | The number a macro stands for.
    Macro Text
  | Literal Integer
  | -- | A call of a macro other than a version test.
    Call

-- | What an expression evaluates to, if anything.
valueOf :: Macros -> Expr -> Maybe Integer
valueOf macros = \case
  Or a b
    | any true [value a, value b] -> Just 1
    | all false [value a, value b] -> Just 0
    | otherwise -> Nothing
  And a b
    | any false [value a, value b] -> Just 0
    | all true [value a, value b] -> Just 1
    | otherwise -> Nothing
  Not a -> fromBool . (== 0) <$> value a
  Compared test a b -> fromBool <$> (test <$> value a <*> value b)
  Defined n -> fromBool <$> isDefined macros n
  Reaches n v -> fromBool <$> reaches macros n v
  Macro n -> numberOf macros n
  Literal n -> Just n
  Call -> Nothing
  where
    value = valueOf macros
    true = maybe False (/= 0)
    false = maybe False (== 0)
    fromBool b = if b then 1 else 0

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
type Reading = Maybe (Expr, [Token])

orExpr :: [Token] -> Reading
orExpr ts = do
  (left, rest) <- andExpr ts
  more left rest
  where
    more left = \case
      Punct "||" : rest -> do
        (right, rest') <- andExpr rest
        more (Or left right) rest'
      rest -> Just (left, rest)

andExpr :: [Token] -> Reading
andExpr ts = do
  (left, rest) <- compared ts
  more left rest
  where
    more left = \case
      Punct "&&" : rest -> do
        (right, rest') <- compared rest
        more (And left right) rest'
      rest -> Just (left, rest)

compared :: [Token] -> Reading
compared ts = do
  (left, rest) <- unary ts
  case rest of
    Punct op : rest' | Just test <- comparison op -> do
      (right, rest'') <- unary rest'
      let e = fromMaybe (Compared test left right) (reaching op left right)
      pure (e, rest'')
    _ -> Just (left, rest)
  where
    comparison = \case
      "==" -> Just (==)
      "!=" -> Just (/=)
      "<" -> Just (<)
      ">" -> Just (>)
      "<=" -> Just (<=)
      ">=" -> Just (>=)
      _ -> Nothing

-- | A macro compared with a number, as what it asks of the macro's number.
reaching :: Text -> Expr -> Expr -> Maybe Expr
reaching op (Macro n) (Literal m) = case op of
  ">=" -> Just (Reaches n [m])
  ">" -> Just (Reaches n [m + 1])
  "<" -> Just (Not (Reaches n [m]))
  "<=" -> Just (Not (Reaches n [m + 1]))
  "==" -> Just exactly
  "!=" -> Just (Not exactly)
  _ -> Nothing
  where
    exactly = And (Reaches n [m]) (Not (Reaches n [m + 1]))
reaching _ _ _ = Nothing

unary :: [Token] -> Reading
unary = \case
  Punct "!" : rest -> do
    (e, rest') <- unary rest
    pure (Not e, rest')
  Punct "(" : rest -> do
    (e, rest') <- orExpr rest
    case rest' of
      Punct ")" : rest'' -> Just (e, rest'')
      _ -> Nothing
  Name "defined" : rest -> case rest of
    Name n : rest' -> Just (Defined n, rest')
    Punct "(" : Name n : Punct ")" : rest' -> Just (Defined n, rest')
    _ -> Nothing
  Name n : Punct "(" : rest -> do
    (arguments, rest') <- argumentList rest
    pure $ case arguments of
      Just v | "MIN_VERSION_" `T.isPrefixOf` n -> (Reaches n v, rest')
      _ -> (Call, rest')
  Name n : rest -> Just (Macro n, rest)
  Number n : rest -> Just (Literal n, rest)
  _ -> Nothing

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
