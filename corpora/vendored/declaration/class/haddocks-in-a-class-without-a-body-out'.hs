module Data.CoerceSubst where

class
    Substitute
        (from :: Type)
        (to :: Type)
    -- ^ the type substituted

    -- ^ what it is substituted with

class Known a -- ^ the type known

class Narrowed a b | a -> b
    -- | a class with no body
