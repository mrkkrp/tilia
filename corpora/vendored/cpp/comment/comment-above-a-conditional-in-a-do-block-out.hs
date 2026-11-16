{-# LANGUAGE CPP #-}

module Garden.Pump where

water :: IO ()
water = do
-- A pump on mains power is switched on before the valves open.
#ifdef MAINS
  let pump = True
#else
  let pump = False
#endif
  print pump
