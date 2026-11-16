{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Circuit.Gate where

data (:&) a b = Gate a b
{-# ANN type (:&) "gate" #-}

pattern (:&&) :: a -> b -> a :& b
pattern x :&& y = Gate x y

{-# COMPLETE (:&&) :: (:&) #-}

(<&>) :: Bool -> Bool -> Bool
(<&>) = (&&)
{-# INLINE (<&>) #-}
{-# ANN (<&>) "conjunction" #-}
