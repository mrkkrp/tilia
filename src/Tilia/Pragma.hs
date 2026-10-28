{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pragma-related helpers.
module Tilia.Pragma
  ( movesPositions,
    effectiveExtensions,
    lookupExtension,
    lookupEdition,
    applyExtensionName,
    languageNames,
    sameInForce,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as Set
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
  -- | The module's source, read here for its @LANGUAGE@, @OPTIONS_GHC@ and
  -- @OPTIONS@ pragmas alone.
  Text ->
  [Extension]
effectiveExtensions package =
  foldl' switch package . concatMap pragmaSwitches . pragmaBodies
  where
    pragmaSwitches body =
      let (keyword, rest) = T.break (== ' ') body
       in switchesOf (T.toUpper keyword) rest

-- | Do two orders of the same pragmas, each given by its name and body,
-- leave the same extensions on?
--
-- Where none of them turns an extension off, every order turns on the same.
sameInForce :: [(Text, Text)] -> [(Text, Text)] -> Bool
sameInForce a b = not (turnsOff a) || inForce a == inForce b
  where
    turnsOff = foldl' (switchWith (const id) (\_ _ -> True)) False . switches
    inForce = foldl' (switchWith Set.insert Set.delete) Set.empty . switches
    switches = concatMap (uncurry switchesOf)

-- | What a pragma of this name and body turns on and off.
switchesOf :: Text -> Text -> [GHC.OnOff Extension]
switchesOf name body = case name of
  "LANGUAGE" -> mapMaybe nameSwitch (languageNames body)
  "OPTIONS_GHC" -> concatMap flagSwitches (T.words body)
  "OPTIONS" -> concatMap flagSwitches (T.words body)
  _ -> []

-- | Turn on the extension a name spells, or off where the name puts @No@ in
-- front of one, leaving the set as it is for a name that spells neither.
applyExtensionName :: [Extension] -> Text -> [Extension]
applyExtensionName acc = maybe acc (switch acc) . nameSwitch

-- | The extension a name turns on, or off where it puts @No@ in front of one.
nameSwitch :: Text -> Maybe (GHC.OnOff Extension)
nameSwitch name = case T.stripPrefix "No" name >>= lookupExtension of
  Just off -> Just (GHC.Off off)
  Nothing -> GHC.On <$> lookupExtension name

-- | What a flag of an @OPTIONS_GHC@ or @OPTIONS@ pragma turns on and off.
flagSwitches :: Text -> [GHC.OnOff Extension]
flagSwitches flag
  | Just name <- T.stripPrefix "-X" flag = maybeToList (nameSwitch name)
  | flag == "-cpp" = [GHC.On Cpp]
  | flag == "-fglasgow-exts" = GHC.On <$> GHC.glasgowExtsFlags
  | flag == "-fno-glasgow-exts" = GHC.Off <$> GHC.glasgowExtsFlags
  | Just name <- T.stripPrefix "-fno-" flag = GHC.Off <$> oldFlag name
  | Just name <- T.stripPrefix "-f" flag = GHC.On <$> oldFlag name
  | otherwise = []
  where
    oldFlag name = maybeToList (Map.lookup name extensionsByOldFlag)

-- | Turn an extension on or off in a list kept in the order extensions were
-- turned on in.
switch :: [Extension] -> GHC.OnOff Extension -> [Extension]
switch = switchWith turnOn (filter . (/=))
  where
    turnOn e acc = if e `elem` acc then acc else acc <> [e]

-- | Turn an extension on, and then what it implies, or turn it off alone, as
-- GHC does.
switchWith ::
  -- | How to turn one extension on.
  (Extension -> a -> a) ->
  -- | How to turn one extension off.
  (Extension -> a -> a) ->
  -- | What is on so far.
  a ->
  -- | What to turn on or off.
  GHC.OnOff Extension ->
  a
switchWith on off acc = \case
  GHC.On e ->
    foldl' (switchWith on off) (on e acc) (Map.findWithDefault [] e impliedBy)
  GHC.Off e -> off e acc

-- | What GHC turns on or off besides when it turns an extension on.
impliedBy :: Map Extension [GHC.OnOff Extension]
impliedBy =
  Map.fromListWith
    (flip (<>))
    [(from, [implied]) | (from, implied) <- GHC.impliedXFlags]

-- | The extensions a @LANGUAGE@ pragma names, as written.
languageNames :: Text -> [Text]
languageNames = filter (not . T.null) . fmap T.strip . T.splitOn ","

-- | Parse the name of an edition of the language, such as @GHC2021@.
lookupEdition :: Text -> Maybe GHC.Language
lookupEdition name =
  lookup name [(T.pack (show e), e) | e <- [minBound .. maxBound]]

-- | Parse an extension name.
lookupExtension :: Text -> Maybe Extension
lookupExtension name = Map.lookup name extensionsByName

-- | Every extension this compiler knows, by the name one writes in a
-- pragma.
extensionsByName :: Map Text Extension
extensionsByName = byFlagName GHC.xFlags

-- | Every extension an old @-f@ flag such as @-fbang-patterns@ turns on, by
-- the flag without its @-f@.
extensionsByOldFlag :: Map Text Extension
extensionsByOldFlag = byFlagName GHC.fLangFlags

-- | Extensions by the names of the flags that turn them on.
byFlagName :: [GHC.FlagSpec Extension] -> Map Text Extension
byFlagName flags =
  Map.fromList [(T.pack (GHC.flagSpecName f), GHC.flagSpecFlag f) | f <- flags]

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
