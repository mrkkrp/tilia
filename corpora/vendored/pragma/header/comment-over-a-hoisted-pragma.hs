-- | Cooling the cache.

{-# LANGUAGE OverloadedStrings #-}

-- eviction order is Cache.Warm's business
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cache.Cool where

cool :: IO ()
cool = pure ()
