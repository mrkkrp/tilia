-- A declaration written right under a group stays in it where it relates
-- to any declaration of the group, not only to the first or the last.
module Strict where

toStrict :: L.ByteString -> B.ByteString
fromStrict :: B.ByteString -> L.ByteString
toStrict = L.toStrict
fromStrict = L.fromStrict
{-# INLINE toStrict #-}
{-# INLINE fromStrict #-}
