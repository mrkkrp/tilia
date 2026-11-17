{-# LANGUAGE CPP #-}

-- A conditional that begins in a where clause and goes on into the
-- declarations after it stays one conditional.
module Data.Witness where

withWitness :: Witness a -> (Known a => r) -> r
withWitness w k = go k
  where
    go :: (Known a => r) -> r
#if MIN_VERSION_base(4,17,0)
    go = withDict @(Known a) w
#else
    go r = unsafeCoerce (Wrap r) w

-- the dictionary of a class with one method is that method
#if __GLASGOW_HASKELL__ >= 810
type Wrap :: Type -> Type -> Type
#endif
newtype Wrap a r = Wrap (Known a => r)
#endif

-- Without an #else.
render :: Witness a -> String
render w = label w
  where
    label _ = "witness"
#if WITNESS_DEBUG
    debug = show . Debugged

#if __GLASGOW_HASKELL__ >= 810
type Debugged :: Type -> Type
#endif
newtype Debugged a = Debugged (Witness a)
  deriving (Show)
#endif
