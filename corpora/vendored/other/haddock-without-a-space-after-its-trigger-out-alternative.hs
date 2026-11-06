module Options where

data Options = Options
  { recover :: Bool
  -- ^ Recover from errors.
  --
  -- Invalid input is taken literally.
  , strict :: Bool
  }

parse ::
  -- | Options to parse with.
  --
  --  Strict ones reject what others let through.
  Options ->
  String ->
  Maybe Int
parse _ _ = Nothing
