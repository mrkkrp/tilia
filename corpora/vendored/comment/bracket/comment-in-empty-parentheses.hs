{-# LANGUAGE UnboxedTuples #-}

module Kiln.Vent where

open :: Valve -> ( {- nothing to report -} )
open valve = vent ({- no pressure -}) valve

close :: Valve -> ( -- nothing to report
  )
close _ = ( -- not yet
  )

purge :: (Monad m) => ( {- done -} )
purge = undefined

vent :: ( {- no pressure -} ) -> Valve -> ()
vent ( {- no pressure -} ) _ = ()

drain :: (# #) -> Int
drain (# {- nothing to drain -} #) = 0

level :: Int
level = 1
