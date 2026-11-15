{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Stack.Level where

data Couple a b = Couple a b

type Levels = Int ': Bool ': '[]

type Paired = Int '`Couple` Bool

type Push = '(:)
