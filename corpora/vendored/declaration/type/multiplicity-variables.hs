{-# LANGUAGE LinearTypes #-}

module Resource.Handle where

consume :: Handle %1 -> ()
consume = undefined

relay :: forall m a. (a %m -> a) -> a %m -> a
relay f x = f x

transfer ::
  Handle %m ->
  Buffer %1 ->
  IO ()
transfer = undefined
