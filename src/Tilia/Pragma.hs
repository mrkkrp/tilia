{-# LANGUAGE OverloadedStrings #-}

-- | Pragma-related helpers.
module Tilia.Pragma
  ( movesPositions,
    effectiveExtensions,
    lookupExtension,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension (..))

-- | Recognize @{-# LINE #-}@ and @{-# COLUMN #-}@ pragmas.
movesPositions :: Text -> Bool
movesPositions = any positional . pragmaBodies
  where
    positional body =
      T.toUpper (T.takeWhile (/= ' ') body) `elem` ["LINE", "COLUMN"]

-- | The extensions actually in force in a module.
effectiveExtensions ::
  -- | What the package the module belongs to puts in force, which is its
  -- @default-language@ and @default-extensions@ already resolved into a
  -- set.
  [Extension] ->
  -- | The module's source, read here for its @LANGUAGE@ pragmas alone.
  Text ->
  [Extension]
effectiveExtensions package
  | null package = pragmasOver onUnlessRefused
  | otherwise = pragmasOver package

-- | The extensions that are on until something says otherwise.
onUnlessRefused :: [Extension]
onUnlessRefused = [ImplicitPrelude]

-- | Apply a module's @LANGUAGE@ pragmas to a starting set.
pragmasOver :: [Extension] -> Text -> [Extension]
pragmasOver initial = foldl' apply initial . concatMap pragmaNames . pragmaBodies
  where
    apply acc name = case T.stripPrefix "No" name >>= lookupExtension of
      Just off -> filter (/= off) acc
      Nothing -> case lookupExtension name of
        Just on | on `notElem` acc -> acc <> [on]
        _ -> acc
    pragmaNames body =
      let (keyword, names) = T.break (== ' ') body
       in if T.toUpper keyword == "LANGUAGE"
            then filter (not . T.null) (fmap T.strip (T.splitOn "," names))
            else []

-- | Parse an extension name.
lookupExtension :: Text -> Maybe Extension
lookupExtension name = Map.lookup name extensionsByName

-- | Every extension this compiler knows, by the name one writes in a
-- pragma.
extensionsByName :: Map Text Extension
extensionsByName =
  Map.fromList
    [(T.pack (GHC.flagSpecName f), GHC.flagSpecFlag f) | f <- GHC.xFlags]

-- | What every @{-# … #-}@ in a module has between its braces, each on one
-- line.
pragmaBodies :: Text -> [Text]
pragmaBodies = go
  where
    go = step . T.dropWhile (\c -> c /= '{' && c /= '-' && c /= '"')

    step t
      | T.null t = []
      | Just body <- T.stripPrefix "{-#" t = case T.breakOn "#-}" body of
          (_, after) | T.null after -> []
          (inner, after) -> T.unwords (T.words inner) : go (T.drop 3 after)
      | Just after <- T.stripPrefix "{-" t = go (skipBlock (1 :: Int) after)
      | opensLineComment t = go (T.dropWhile (/= '\n') t)
      | Just after <- T.stripPrefix "\"" t = go (skipString after)
      | otherwise = go (T.drop 1 t)

    opensLineComment t = case T.stripPrefix "--" t of
      Nothing -> False
      Just rest -> maybe True (not . symbolic . fst) (T.uncons rest)

    symbolic c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

    skipBlock 0 t = t
    skipBlock n t0 = inBlock n (T.dropWhile (\c -> c /= '{' && c /= '-') t0)

    inBlock n t
      | T.null t = t
      | Just after <- T.stripPrefix "{-" t = skipBlock (n + 1) after
      | Just after <- T.stripPrefix "-}" t = skipBlock (n - 1) after
      | otherwise = skipBlock n (T.drop 1 t)

    skipString = inString . T.dropWhile (\c -> c /= '"' && c /= '\\')

    inString t
      | T.null t = t
      | Just after <- T.stripPrefix "\\" t = skipString (T.drop 1 after)
      | Just after <- T.stripPrefix "\"" t = after
      | otherwise = skipString (T.drop 1 t)
