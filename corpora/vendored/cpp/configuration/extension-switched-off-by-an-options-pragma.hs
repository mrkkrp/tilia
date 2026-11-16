{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# OPTIONS_GHC -XNoOverloadedRecordDot #-}
#endif
module Shop.Receipt (subtotal) where

subtotal line = line.price * line.quantity
