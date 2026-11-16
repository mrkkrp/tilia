{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Stock.Count where

data Count = None | More Count

type family (a :: Count) + (b :: Count) :: Count where
  'None + b = b
  'More a + b = 'More (a + b)

type family (a :: Count) `AtMost` (b :: Count) :: Bool where
  'None `AtMost` b = 'True
  'More a `AtMost` 'None = 'False
  'More a `AtMost` 'More b = a `AtMost` b

type family ((a :: Count) `Within` (b :: Count)) (c :: Count) :: Bool where
  (a `Within` b) c = a `AtMost` c

type family
  (a :: Count)
    `Times` (b :: Count) ::
    Count
