{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The settings a project writes in its @tilia.toml@.
module Tilia.Settings
  ( Settings (..),
    CommaStyle (..),
    ArrowStyle (..),
    defaultSettings,
    alternativeSettings,
    settingsFile,
    readSettings,
    parseSettings,
    SettingsProblem (..),
    describeSettingsProblem,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.ByteString qualified as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read qualified as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Tilia.Utils (tshow)

-- | What a project can set.
data Settings = Settings
  { -- | The column past which what can break is broken.
    settingsColumnLimit :: !Int,
    -- | Where a list laid out on several lines puts its commas.
    settingsCommaStyle :: !CommaStyle,
    -- | Where a type laid out on several lines puts its arrows.
    settingsArrowStyle :: !ArrowStyle
  }
  deriving (Eq, Show)

-- | Where a list laid out on several lines puts its commas.
data CommaStyle
  = -- | At the end of each line but the last.
    TrailingCommas
  | -- | At the start of each line but the first, two columns left of the
    -- items.
    LeadingCommas
  deriving (Eq, Show)

-- | Where a type laid out on several lines puts its @::@, @=>@ and @->@,
-- and the @.@ of a @forall@.
data ArrowStyle
  = -- | At the end of each line but the last.
    TrailingArrows
  | -- | At the start of each line but the first.
    LeadingArrows
  deriving (Eq, Show)

-- | What a project that sets nothing gets.
defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsColumnLimit = 80,
      settingsCommaStyle = TrailingCommas,
      settingsArrowStyle = TrailingArrows
    }

-- | Settings that differ from 'defaultSettings' in every one but the column
-- limit, which the corpora are formatted with as well.
alternativeSettings :: Settings
alternativeSettings =
  defaultSettings
    { settingsCommaStyle = LeadingCommas,
      settingsArrowStyle = LeadingArrows
    }

-- | The file a project's settings are in, relative to its root.
settingsFile :: FilePath
settingsFile = "tilia.toml"

-- | Why a project's settings cannot be used.
data SettingsProblem
  = -- | The file cannot be read, and why.
    SettingsUnreadable Text
  | -- | A line of it, by its number, says something it cannot, and why.
    SettingsMalformed Int Text
  deriving (Eq, Show)

-- | Say what is wrong with a project's settings.
describeSettingsProblem ::
  -- | The problem.
  SettingsProblem ->
  Text
describeSettingsProblem = \case
  SettingsUnreadable why -> why
  SettingsMalformed n why -> "line " <> tshow n <> ": " <> why

-- | Read the settings of a project, or the defaults where it has no
-- 'settingsFile'.
readSettings ::
  -- | The project's root.
  FilePath ->
  IO (Either SettingsProblem Settings)
readSettings root = do
  let path = root </> settingsFile
  there <- doesFileExist path
  if not there
    then pure (Right defaultSettings)
    else
      try (BS.readFile path) >>= \case
        Left (e :: IOException) -> pure (Left (SettingsUnreadable (tshow e)))
        Right bytes -> pure (parseSettings (T.decodeUtf8Lenient bytes))

-- | Read the settings a file holds: TOML, but only lines of @key = value@
-- where a value is @true@, @false@ or a whole number.
parseSettings ::
  -- | The file's text.
  Text ->
  Either SettingsProblem Settings
parseSettings written = do
  entries <- catMaybes <$> traverse entry (zip [1 ..] (T.lines written))
  fst <$> foldM set (defaultSettings, Map.empty) entries
  where
    entry (n, l) = case T.strip (T.takeWhile (/= '#') l) of
      "" -> Right Nothing
      t
        | (key, rest) <- T.breakOn "=" t,
          Just v <- T.stripPrefix "=" rest,
          bare (T.strip key) ->
            Just . (n,T.strip key,) <$> value n (T.strip v)
        | otherwise -> malformed n "expected a line of key = value"
    bare key = not (T.null key) && T.all keyChar key
    keyChar c =
      isAsciiLower c || isAsciiUpper c || isDigit c || c == '-' || c == '_'
    value n = \case
      "true" -> Right (Flag True)
      "false" -> Right (Flag False)
      v
        | Right (i, "") <- T.signed T.decimal v,
          not (leadingZero v) ->
            Right (Number i)
        | otherwise -> malformed n "a value is true, false or a whole number"
    leadingZero v = case T.unpack (T.dropWhile (`elem` ['+', '-']) v) of
      '0' : _ : _ -> True
      _ -> False
    set (settings, seen) (n, key, v) = case Map.lookup key seen of
      Just earlier ->
        malformed n (key <> " is already set on line " <> tshow earlier)
      Nothing -> (,Map.insert key n seen) <$> setting settings n key v
    setting settings n key v = case (key, v) of
      ("column-limit", Number i)
        | 0 < i && i <= toInteger (maxBound :: Int) ->
            Right settings{settingsColumnLimit = fromInteger i}
      ("column-limit", _) ->
        malformed n "column-limit is a whole number greater than 0"
      ("leading-commas", Flag b) ->
        Right
          settings
            { settingsCommaStyle = if b then LeadingCommas else TrailingCommas
            }
      ("leading-commas", _) -> malformed n "leading-commas is true or false"
      ("leading-arrows", Flag b) ->
        Right
          settings
            { settingsArrowStyle = if b then LeadingArrows else TrailingArrows
            }
      ("leading-arrows", _) -> malformed n "leading-arrows is true or false"
      _ -> malformed n ("there is no setting " <> key)
    malformed n = Left . SettingsMalformed n

-- | A value a setting can take.
data Value
  = -- | @true@ or @false@.
    Flag Bool
  | -- | A whole number.
    Number Integer
