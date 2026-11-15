module Terminal.Time where

class Timed a where
  timeType ::
    Maybe Word ->
    Bool ->
    a
  -- TODO interval type

class Zoned a where
  zonedType :: Maybe Word -> Bool {-^ With time zone -} -> a
  -- TODO interval type

instance Timed Int where
  timeType precision zoned =
    if zoned then 1 else 0
  -- TODO use the precision

clock :: Int
clock = 1
