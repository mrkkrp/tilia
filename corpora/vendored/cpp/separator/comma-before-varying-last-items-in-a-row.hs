{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE DataKinds #-}
#endif

module Shapes.Generic where

-- Where the module is formatted once per answer to a question, items under
-- that question come last in one answer and are followed by more in the
-- other. The items before them still come out once, as written.
data Shape = Circle | Square
  deriving ( Eq, Ord
#if __GLASGOW_HASKELL__ >= 704
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Generic1
#endif
           )
