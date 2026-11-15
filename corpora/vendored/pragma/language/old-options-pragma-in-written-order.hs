{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Kiln.Fire where

-- GHC reads OPTIONS as it reads OPTIONS_GHC, so the two keep the order they
-- are written in among each other.
fire :: Int
fire = 1
