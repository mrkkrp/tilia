{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
#ifdef PLAIN_RECORDS
{-# LANGUAGE NoOverloadedRecordDot #-}
#endif

module Shop.Discount (discount) where

#ifdef SALE
#ifdef PLAIN_RECORDS
discount item = item . price / 2
#else
discount item = item.price / 2
#endif
#else
#ifdef PLAIN_RECORDS
discount item = item . price
#else
discount item = item.price
#endif
#endif
