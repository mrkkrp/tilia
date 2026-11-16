module Functor.Compose where

data (f :.: g) a = Compose (f (g a))

newtype (f `Over` g) a = Over (f (g a))

data (f :+: g) a = InLeft (f a) | InRight (g a)

data (f :*: g) a = f a :*: g a

data
  ( f
      :=> g
  )
    a
  = Arrow (f a -> g a)
  | Constant (g a)

data
  ( f
      `Then` g
  )
    a
    b = Then (f a) (g b)
