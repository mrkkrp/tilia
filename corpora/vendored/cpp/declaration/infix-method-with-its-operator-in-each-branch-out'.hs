{-# LANGUAGE CPP #-}

-- An operator written in each branch, between operands written once around
-- the conditional, takes the operand after it into each branch, where the
-- same question asked above picks the instance the method belongs to.
module Weather.Rainfall (Rainfall (..)) where

data Rainfall = Rainfall !Int !Int

instance Monoid Rainfall where
    mempty = Rainfall 0 0

#ifdef WITH_SEMIGROUP
instance Semigroup Rainfall where
#endif
    (Rainfall am pm)
#ifdef WITH_SEMIGROUP
        <> (Rainfall am' pm') = Rainfall (am + am') (max pm pm')
#else
        `mappend` (Rainfall am' pm') = Rainfall (am + am') (max pm pm')
#endif
