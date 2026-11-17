{-# LANGUAGE TypeFamilies #-}

-- A pragma naming a constructor or a field of a data instance stays next to
-- the instance, as one naming a member of a type does.
module Store.Key where

data family Key c

data instance Key Int = IntKey Int
{-# DEPRECATED IntKey "Use Key Word instead." #-}

newtype instance Key Char = CharKey{charKey :: Char}
{-# DEPRECATED charKey "Use Key Word instead." #-}
