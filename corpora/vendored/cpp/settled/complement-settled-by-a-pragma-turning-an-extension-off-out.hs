{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# LANGUAGE NoOverloadedRecordDot #-}
#endif

module Shop.Basket where

#ifndef PLAIN_RECORDS
total basket = sum basket.prices
#endif
