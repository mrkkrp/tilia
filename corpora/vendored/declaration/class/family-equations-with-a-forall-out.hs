{-# LANGUAGE TypeFamilies #-}

module Container.Inner where

type family Inner c where
  forall a. Inner [a] = a
  forall k v. Inner (Map k v) = v
  Inner c = c

type family Outer c

type instance forall a. Outer (Maybe a) = a

type instance forall e i. Outer (Indexed i e) = (i, e)
