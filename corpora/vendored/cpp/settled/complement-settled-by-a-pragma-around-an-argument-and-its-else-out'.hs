{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# LANGUAGE NoOverloadedRecordDot #-}
#endif

module Shop.Basket where

total basket =
    sum
#ifndef PLAIN_RECORDS
        basket.prices
#else
        (prices basket)
#endif
