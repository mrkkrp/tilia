{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# LANGUAGE NoOverloadedRecordDot #-}
#endif
module Shop.Discount (discount) where

#ifdef SALE
discount item = item.price / 2
#else
discount item = item.price
#endif
