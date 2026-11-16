{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# LANGUAGE NoOverloadedRecordDot #-}
#endif

module Shop.Basket (total) where

#ifdef DEBUG
import Debug.Trace (trace)
#endif

#ifdef PLAIN_RECORDS
total basket = sum basket . prices
#else
total basket = sum basket.prices
#endif
