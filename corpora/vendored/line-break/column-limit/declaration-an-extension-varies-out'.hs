-- A declaration that an extension switched behind a conditional varies goes
-- whole into each branch, and is broken there where it runs past column 80.
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# LANGUAGE NoOverloadedRecordDot #-}
#endif

module Shop.Basket (total) where

#ifdef PLAIN_RECORDS
total basket =
    sum basket . prices
        + sum basket . taxes
        + sum basket . fees
        + basket . postage
#else
total basket =
    sum basket.prices + sum basket.taxes + sum basket.fees + basket.postage
#endif
