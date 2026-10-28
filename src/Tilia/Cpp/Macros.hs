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

-- | What is known about macros, either from a build plan or implied by the
-- answers a configuration gives its conditionals.
data Macros = Macros
  { -- | The versions from the build plan that @MIN_VERSION_@ macros compare
    -- with, by macro name, for packages the plan holds at one version only,
    -- and the compiler's version for @MIN_VERSION_GLASGOW_HASKELL@.
    macroExactVersions :: Map Text [Integer],
    -- | The numbers that @__GLASGOW_HASKELL__@ and its patch levels expand
    -- to, from the build plan.
    macroExactNumbers :: Map Text Integer,
    -- | The macros known to be defined as implied by the answers that reach
    -- a configuration.
    macroDefined :: Set Text,
    -- | The macros known not to be defined: from the build plan, those of
    -- the compilers other than the one it is for, like @__MHS__@, and
    -- otherwise implied by the answers that reach a configuration.
    macroUndefined :: Set Text,
    -- | Every package the build plan holds, at however many versions, as
    -- their version macros spell them: the @VERSION_@ and @MIN_VERSION_@
    -- macros of any other package are not defined. 'Nothing' without a
    -- build plan.
    macroPlannedPackages :: Maybe (Set Text),
    -- | Implied by answers, the version, or number, each macro is known to
    -- reach, a number being a version of one part.
    macroLowerBounds :: Map Text [Integer],
    -- | Implied by answers, the version, or number, each macro is known to
    -- stay below.
    macroUpperBounds :: Map Text [Integer]
  }
  deriving (Eq, Show)

instance Semigroup Macros where
  a <> b =
    Macros
      { macroExactVersions = macroExactVersions a <> macroExactVersions b,
        macroExactNumbers = macroExactNumbers a <> macroExactNumbers b,
        macroDefined = macroDefined a <> macroDefined b,
        macroUndefined = macroUndefined a <> macroUndefined b,
        macroPlannedPackages =
          macroPlannedPackages a <|> macroPlannedPackages b,
        macroLowerBounds =
          Map.unionWith max (macroLowerBounds a) (macroLowerBounds b),
        macroUpperBounds =
          Map.unionWith min (macroUpperBounds a) (macroUpperBounds b)
      }

instance Monoid Macros where
  mempty =
    Macros
      { macroExactVersions = Map.empty,
        macroExactNumbers = Map.empty,
        macroDefined = Set.empty,
        macroUndefined = Set.empty,
        macroPlannedPackages = Nothing,
        macroLowerBounds = Map.empty,
        macroUpperBounds = Map.empty
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
        | h -> mempty{macroLowerBounds = Map.singleton n (trimmed v)}
        | otherwise -> mempty{macroUpperBounds = Map.singleton n (trimmed v)}
      Macro n | h -> mempty{macroDefined = Set.singleton n}
      _ -> mempty

-- | Whether a macro is defined, where that is known.
isDefined :: Macros -> Text -> Maybe Bool
isDefined macros n
  | Map.member n (macroExactVersions macros)
      || Map.member n (macroExactNumbers macros)
      || Set.member n (macroDefined macros) =
      Just True
  | Set.member n (macroUndefined macros) = Just False
  | Just held <- macroPlannedPackages macros,
    Just p <- T.stripPrefix "VERSION_" n <|> T.stripPrefix "MIN_VERSION_" n,
    Set.notMember p held =
      Just False
  | otherwise = Nothing

-- | The number a macro stands for, where that is known.
numberOf :: Macros -> Text -> Maybe Integer
numberOf macros n = case isDefined macros n of
  Just False -> Just 0
  _ -> Map.lookup n (macroExactNumbers macros)

-- | Does a macro's version, or number, reach this one, where what is known
-- settles it?
reaches :: Macros -> Text -> [Integer] -> Maybe Bool
reaches macros n wanted = case held of
  Just v -> Just (trimmed v >= w)
  Nothing
    | any (>= w) (Map.lookup n (macroLowerBounds macros)) -> Just True
    | any (<= w) (Map.lookup n (macroUpperBounds macros)) -> Just False
    | otherwise -> Nothing
  where
    w = trimmed wanted
    held =
      Map.lookup n (macroExactVersions macros) <|> (pure <$> numberOf macros n)

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

orExpr :: [Token] -> Maybe (Expr, [Token])
orExpr ts = do
  (left, rest) <- andExpr ts
  more left rest
  where
    more left = \case
      Punct "||" : rest -> do
        (right, rest') <- andExpr rest
        more (Or left right) rest'
      rest -> Just (left, rest)

andExpr :: [Token] -> Maybe (Expr, [Token])
andExpr ts = do
  (left, rest) <- compared ts
  more left rest
  where
    more left = \case
      Punct "&&" : rest -> do
        (right, rest') <- compared rest
        more (And left right) rest'
      rest -> Just (left, rest)

compared :: [Token] -> Maybe (Expr, [Token])
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

unary :: [Token] -> Maybe (Expr, [Token])
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
