module Storage.Table where

import Data.Kind (Type)

data
  Table
    key
    value
  = Table [(key, value)]

newtype Index key
              value = Index (key -> Maybe value)

data
  {-# CTYPE "storage.h" "struct page" #-}
  Page = Page Int

data Shard ::
  Type -> Type where
  Shard :: Int -> Shard a

data Cursor
  :: Type
  -> Type
  where
    Start :: Cursor a
    Next :: a -> Cursor a -> Cursor a

data Journal entry
  where
  Empty :: Journal entry
  Append :: entry -> Journal entry -> Journal entry
