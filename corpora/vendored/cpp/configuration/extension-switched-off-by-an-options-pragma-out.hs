{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# OPTIONS_GHC -XNoOverloadedRecordDot #-}
#endif

module Shop.Receipt (subtotal) where

#ifdef PLAIN_RECORDS
subtotal line = line . price * line . quantity
#else
subtotal line = line.price * line.quantity
#endif
