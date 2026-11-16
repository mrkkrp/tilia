module Route.Join where

class (:<>) a b

class
  (:<|>)
    a
    b

class a :+: b

class (f :. g) a where
  through :: f (g a) -> a

class (f `Under` g) a

class
  ( path
      :/ rest
  )
    method
