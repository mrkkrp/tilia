{-# LANGUAGE TypeFamilies #-}

module Container where

type family Outer c

type instance
    forall e i
     . Outer (Indexed i e) = (i, e)
