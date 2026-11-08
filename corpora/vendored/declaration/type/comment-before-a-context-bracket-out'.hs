module Server.Route where

-- A comment written before the closing parenthesis of a context stays
-- inside it.
route ::
  ( Monad m
  , MonadIO m -- for the effects
  -- that run around each request
  ) =>
  Handler m ->
  m ()
route = undefined

instance
  ( Show a
  , Eq a
  -- and nothing else
  ) =>
  Render (Box a)
