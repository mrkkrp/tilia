{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# OPTIONS_GHC -Wall #-}

module Kiln.Glaze where

-- NoMonoLocalBinds has to stay under TypeFamilies, which would turn
-- MonoLocalBinds back on under it, but the pragmas switching neither of the
-- two sort around them.
type family Glaze a

coat :: Int -> Int
coat !n = n + 1
