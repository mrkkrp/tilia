{-# LANGUAGE Arrows #-}

module Parser.Sign where

import Control.Arrow ((<+>))

sign = proc input -> do
           token Plus -< input
           returnA -< 1
      <+> do
           token Minus -< input
           returnA -< negate 1
      <+> do
           returnA -< 1
