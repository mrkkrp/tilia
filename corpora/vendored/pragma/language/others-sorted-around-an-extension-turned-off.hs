{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}
module Kiln.Glaze where

-- NoMonoLocalBinds has to stay under TypeFamilies, which would turn
-- MonoLocalBinds back on under it, but the pragmas switching neither of the
-- two sort around them.
type family Glaze a

coat :: Int -> Int
coat !n = n + 1
