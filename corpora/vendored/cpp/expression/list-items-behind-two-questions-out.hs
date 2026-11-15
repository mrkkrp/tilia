{-# LANGUAGE CPP #-}

module Laws.Mod (laws) where

#ifdef MIN_VERSION_semirings
laws :: (Ring a) => Proxy a -> [Laws]
#else
laws :: Proxy a -> [Laws]
#endif
laws p =
  [ eqLaws p
#ifdef MIN_VERSION_semirings
    , ringLaws p
#endif
#ifdef MIN_VERSION_vector
    , primLaws p
#endif
  ]
