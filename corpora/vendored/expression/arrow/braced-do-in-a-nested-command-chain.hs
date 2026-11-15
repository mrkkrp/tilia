{-# LANGUAGE Arrows #-}

module Circuit.Choice where

import Control.Arrow ((<+>), (|||))

pick = proc x -> (sense -< x) <+> do { probe -< x } ||| (fallback -< x)
