module Pipe.Stage where

class Composable p where
    compose :: p b c -> p a b -> p a c

instance Composable (->) where
    compose = (.)

type Reader r = (->) r

newtype Stage a = Stage ((->) a a)
