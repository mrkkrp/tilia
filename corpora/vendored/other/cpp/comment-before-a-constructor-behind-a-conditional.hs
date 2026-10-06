{-# LANGUAGE CPP #-}

module Route.Vehicle where

data Vehicle =
  Bus {
    busLine :: Int
  }
  -- buses run on every route
#ifdef WITH_TRAMS
  | Tram
#endif
