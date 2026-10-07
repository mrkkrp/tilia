{-# LANGUAGE CPP #-}

module Route.Vehicle where

data Vehicle
  =
#ifdef WITH_TRAMS
  Bus
    { busLine :: Int
    }
  | -- buses run on every route

    Tram
#else
  Bus
  { busLine :: Int
  }
-- buses run on every route
#endif
