module Options where

data Options = Options
  { -- | Recover from errors.
    --
    --  Invalid input is taken literally.
    recover :: Bool,
    strict :: Bool
  }

parse ::
  -- | Options to parse with.
  --
  --  Strict ones reject what others let through.
  Options ->
  String ->
  Maybe Int
parse _ _ = Nothing
