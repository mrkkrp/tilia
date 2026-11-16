{-# LANGUAGE TypeFamilies #-}

module Unit.Convert where

convert :: (from ~ to) => Quantity from -> Quantity to
convert = coerceUnit

sameUnit :: (Unit a ~ Unit b, Show a) => a -> b -> Bool
sameUnit _ _ = True

widen :: (a ~ Int) => a -> Integer
widen = toInteger
