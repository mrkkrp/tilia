module Effect.Handler where

type Natural f g = forall a. f a -> g a

type Handler m n =
  forall x.
  m x ->
  n x

type Lifted m =
  forall a b. (a -> m b) -> [a] -> m [b]
