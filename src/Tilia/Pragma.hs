{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pragma-related helpers.
module Tilia.Pragma
  ( movesPositions,
    Defaults,
    defaultsOf,
    effectiveExtensions,
    lookupExtension,
    lookupEdition,
    nameSwitch,
    flagSwitches,
    optionFlags,
    pragmaFlags,
    languageNames,
    withoutSkippedComments,
    sameInForce,
  )
where

import Control.Monad (guard, (<=<))
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.List (unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (fsLit)
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Parser.Header (toArgs)
import GHC.Types.SrcLoc (mkRealSrcLoc, unLoc)
import Tilia.Utils (tshow)

-- | Recognize @{-# LINE #-}@ and @{-# COLUMN #-}@ pragmas in a module with
-- these extensions in force, and the lines GHC's lexer reads past at the
-- start of a line, but for the @#!@ lines a script begins with.
--
-- Those are the line markers cpp writes, such as @# 40 "M.hs"@, which move
-- positions as a @LINE@ pragma does, and @#pragma@ and @#!@ lines, which
-- leave nothing behind for formatting to print. With CPP on, @#line@ and
-- @#pragma@ lines are directives, which formatting keeps.
movesPositions :: [Extension] -> Text -> Bool
movesPositions extensions =
  or . scanPragmas positional (pastCode extensions) . T.cons '\n' . script
  where
    positional body =
      T.toUpper (T.takeWhile (/= ' ') body) `elem` ["LINE", "COLUMN"]
    script t
      | "#!" `T.isPrefixOf` t = script (T.drop 1 (T.dropWhile (/= '\n') t))
      | otherwise = t

-- | What a component puts in force before the pragmas of a module: its
-- @default-language@ and its @default-extensions@.
data Defaults = Defaults
  { -- | What it turns on and off on top of its edition, in order, and on top
    -- of an edition the module names in its place.
    defaultsSwitches :: [GHC.OnOff Extension],
    -- | What its edition and those switches put in force, worked out once.
    defaultsInForce :: [Extension]
  }
  deriving (Eq, Show)

-- | What a component puts in force, given the extensions of its edition and
-- what it switches on top of them, in order.
defaultsOf :: [Extension] -> [GHC.OnOff Extension] -> Defaults
defaultsOf edition switches = Defaults switches (foldl' switch edition switches)

-- | The extensions actually in force in a module.
--
-- As with GHC, the edition the header names last takes the place of the
-- component's, wherever it is named, and what the component and then the
-- header switch is switched on top of it.
effectiveExtensions ::
  -- | What the component the module belongs to puts in force.
  Defaults ->
  -- | The module's source, read here for the @LANGUAGE@, @OPTIONS_GHC@ and
  -- @OPTIONS@ pragmas of its header alone.
  Text ->
  [Extension]
effectiveExtensions defaults source =
  foldl' switch inForce (concat switches)
  where
    (editions, switches) = unzip (fmap ofPragma (headerPragmaBodies source))
    ofPragma body =
      let (keyword, rest) = T.break (== ' ') body
       in editionsAndSwitches (T.toUpper keyword) rest
    inForce = case concat editions of
      [] -> defaultsInForce defaults
      named ->
        foldl'
          switch
          (GHC.languageExtensions (Just (last named)))
          (defaultsSwitches defaults)

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
switchesOf name = snd . editionsAndSwitches name

-- | The editions a pragma of this name and body names, and what it turns on
-- and off.
editionsAndSwitches :: Text -> Text -> ([GHC.Language], [GHC.OnOff Extension])
editionsAndSwitches name body =
  ( mapMaybe (lookupEdition <=< T.stripPrefix "-X") flags,
    concatMap flagSwitches flags
  )
  where
    flags = pragmaFlags name body

-- | The flags a pragma of this name and body gives GHC: each name of a
-- @LANGUAGE@ pragma after @-X@, and the flags of an options pragma.
pragmaFlags :: Text -> Text -> [Text]
pragmaFlags name body = case name of
  "LANGUAGE" -> ["-X" <> n | n <- languageNames body]
  "OPTIONS_GHC" -> optionFlags body
  "OPTIONS" -> optionFlags body
  _ -> []

-- | The flags of the body of an options pragma, split as GHC splits them.
optionFlags :: Text -> [Text]
optionFlags body =
  either (const (T.words body)) (fmap (T.pack . unLoc)) $
    toArgs (mkRealSrcLoc (fsLit "") 1 1) (T.unpack body)

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
lookupEdition name = Map.lookup name editionsByName

-- | Every edition of the language this compiler knows, by its name.
editionsByName :: Map Text GHC.Language
editionsByName =
  Map.fromList [(tshow e, e) | e <- [minBound .. maxBound]]

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

-- | Carry on past the code of a module with these extensions in force, over
-- its strings, characters and quasi-quotations, up to a line that GHC's
-- lexer reads past and formatting would lose.
pastCode :: [Extension] -> (Text -> [Bool]) -> Text -> [Bool]
pastCode extensions = past
  where
    past go t = case T.uncons t of
      Just ('\n', after) | readPast after -> [True]
      Just ('"', after) -> go (skipString after)
      Just ('\'', after) -> go (skipCharacter after)
      _
        | QuasiQuotes `elem` extensions,
          Just body <- quasiQuotation t ->
            go (T.drop 2 (snd (T.breakOn "|]" body)))
        | otherwise -> go (skipCode t)

    withoutCpp = Cpp `notElem` extensions
    readPast l = case T.uncons l of
      Just ('#', rest) ->
        (withoutCpp && any (`T.isPrefixOf` rest) ["line", "pragma"])
          || "!" `T.isPrefixOf` rest
          || T.any isDigit (T.take 1 (fromMaybe rest (T.stripPrefix " " rest)))
      Just (' ', rest) -> "#!" `T.isPrefixOf` rest
      _ -> False

    quasiQuotation t = do
      (quoter, rest) <- T.span inQuoter <$> T.stripPrefix "[" t
      (modules, name) <- unsnoc (T.splitOn "." quoter)
      guard (all (begins isUpper) modules && begins varStart name)
      T.stripPrefix "|" rest
    inQuoter c = inName c || c == '.'
    begins p = maybe False (p . fst) . T.uncons
    varStart c = isLower c || c == '_'

    skipString = inString . T.dropWhile (\c -> c /= '"' && c /= '\\')

    inString t
      | T.null t = t
      | Just after <- T.stripPrefix "\\" t = skipString (T.drop 1 after)
      | Just after <- T.stripPrefix "\"" t = after
      | otherwise = skipString (T.drop 1 t)

    skipCharacter t = case T.unpack (T.take 2 t) of
      ['\\', _] -> T.drop 1 (T.dropWhile (/= '\'') (T.drop 2 t))
      [c, '\''] | c /= '\'' -> T.drop 2 t
      _ -> t

    skipCode t =
      let (plain, rest) = T.break opens (T.drop 1 t)
          before = maybe (T.head t) snd (T.unsnoc plain)
       in if "'" `T.isPrefixOf` rest && inName before then skipCode rest else rest
    opens c =
      c == '{' || c == '-' || c == '"' || c == '\'' || c == '[' || c == '\n'
    inName c = isAlphaNum c || c == '_' || c == '\''

-- | What the @{-# … #-}@ of a module's header have between their braces, each
-- on one line: those above the first thing that is neither a comment, a
-- directive nor a pragma, which are all GHC reads.
headerPragmaBodies :: Text -> [Text]
headerPragmaBodies = scanPragmas id past
  where
    past go t = case T.uncons t of
      Just (c, _)
        | isSpace c -> go (T.stripStart t)
        | c == '#' -> go (T.dropWhile (/= '\n') t)
      _ -> []

-- | What the given function makes of the bodies of the pragmas of a module,
-- each on one line, carrying on past what is neither a comment nor a pragma
-- as the other given function does.
scanPragmas :: (Text -> a) -> ((Text -> [a]) -> Text -> [a]) -> Text -> [a]
scanPragmas pragma past = go
  where
    go t
      | T.null t = []
      | Just body <- T.stripPrefix "{-#" t = case T.breakOn "#-}" body of
          (_, after) | T.null after -> []
          (inner, after) ->
            pragma (T.unwords (T.words (withoutSkippedComments inner)))
              : go (T.drop 3 after)
      | Just after <- T.stripPrefix "{-" t = go (skipBlock (1 :: Int) after)
      | opensLineComment t = go (T.dropWhile (/= '\n') t)
      | otherwise = past go t

-- | What a pragma holds between its @{-#@ and its @#-}@, without the
-- comments GHC skips in it: those of a @LANGUAGE@ pragma, which then comes
-- out on one line.
withoutSkippedComments :: Text -> Text
withoutSkippedComments inner
  | T.any (\c -> c == '-' || c == '{') inner,
    T.toUpper keyword == "LANGUAGE" =
      T.unwords (T.words (withoutComments inner))
  | otherwise = inner
  where
    keyword = T.takeWhile (not . isSpace) (T.stripStart inner)

-- | A text without its comments, each of which leaves a space behind.
withoutComments :: Text -> Text
withoutComments t
  | T.null t = t
  | Just after <- T.stripPrefix "{-" t =
      " " <> withoutComments (skipBlock (1 :: Int) after)
  | opensLineComment t = withoutComments (T.dropWhile (/= '\n') t)
  | otherwise =
      let (plain, rest) = T.break (\c -> c == '{' || c == '-') (T.drop 1 t)
       in T.take 1 t <> plain <> withoutComments rest

-- | Does a line comment begin here?
opensLineComment :: Text -> Bool
opensLineComment t = case T.stripPrefix "--" t of
  Nothing -> False
  Just rest ->
    maybe True (not . symbolic . fst) (T.uncons (T.dropWhile (== '-') rest))
  where
    symbolic c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

-- | What follows the end of a block comment this deep in comments.
skipBlock :: Int -> Text -> Text
skipBlock 0 t = t
skipBlock n t0 = inBlock (T.dropWhile (\c -> c /= '{' && c /= '-') t0)
  where
    inBlock t
      | T.null t = t
      | Just after <- T.stripPrefix "{-" t = skipBlock (n + 1) after
      | Just after <- T.stripPrefix "-}" t = skipBlock (n - 1) after
      | otherwise = skipBlock n (T.drop 1 t)
