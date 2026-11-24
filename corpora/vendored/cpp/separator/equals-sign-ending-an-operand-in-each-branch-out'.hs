{-# LANGUAGE CPP #-}

-- An infix head whose operator and the operand after it, written over
-- several lines, come in each branch, with the = ending the operand's last
-- line and the body under it.
module Weather.Rainfall (Rainfall (..)) where

data Rainfall = Rainfall !Int !Int

instance Monoid Rainfall where
    mempty = Rainfall 0 0

#ifdef WITH_SEMIGROUP
instance Semigroup Rainfall where
#endif
    (Rainfall am pm)
#ifdef WITH_SEMIGROUP
        <> ( Rainfall
                am'
                pm'
            ) =
            Rainfall (am + am') (max pm pm')
#else
        `mappend` ( Rainfall
                        am'
                        pm'
                    ) =
            Rainfall (am + am') (max pm pm')
#endif
