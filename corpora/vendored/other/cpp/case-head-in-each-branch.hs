{-# LANGUAGE CPP #-}

module Data.Aeson.BetterErrors.Internal (key, keyMay) where

key :: Text -> Object -> Parse Value
key k obj = do
#if MIN_VERSION_aeson(2,0,0)
  case KeyMap.lookup (Key.fromText k) obj of
#else
  case HashMap.lookup k obj of
#endif
    Just v -> pure v
    Nothing -> missing k

keyMay :: Text -> Maybe Object -> Parse (Maybe Value)
keyMay k value = case value of
  Just obj ->
#if MIN_VERSION_aeson(2,0,0)
    case KeyMap.lookup (Key.fromText k) obj of
#else
    case HashMap.lookup k obj of
#endif
      Just v -> pure (Just v)
      Nothing -> pure Nothing
  Nothing -> pure Nothing
