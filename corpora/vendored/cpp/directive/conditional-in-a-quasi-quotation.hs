{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Programs where

import Data.String.Interpolate (i)

source :: String
source =
  [i|
#if __GLASGOW_HASKELL__ >= 901
import qualified Data.Type.Ord
#endif

test :: Proxy x -> Proxy x
test = id
|]
