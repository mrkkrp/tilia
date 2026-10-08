{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Kiln.Glaze where

-- GHC reads the flags of the options pragmas in the order they are written,
-- a later one overriding an earlier one, so they keep that order, and only a
-- pragma written twice in a row is written once.
glaze :: Int
glaze = 1
