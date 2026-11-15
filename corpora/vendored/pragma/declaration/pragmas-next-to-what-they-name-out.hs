{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- A pragma stays next to the declaration it names, whatever kind of
-- declaration that is, but for a fixity declaration.
module Codec.Legacy where

import Data.Kind (Type)

{-# DEPRECATED Pixel "Use Colour instead." #-}
type Pixel = Int
{-# ANN type Pixel "legacy" #-}

{-# WARNING Palette "Palette is partial." #-}
class Palette p where
  shade :: p -> Int

type family Channel c
{-# DEPRECATED Channel "Use Component instead." #-}

{-# DEPRECATED Frame "Use Image instead." #-}
type Frame :: Type
data Frame = Frame

{-# INLINE Black #-}
pattern Black :: Int
pattern Black = 0

foreign import ccall "blend" blend :: Int -> Int -> Int
{-# DEPRECATED blend "Use mix instead." #-}

{-# DEPRECATED (<+>) "Use mix instead." #-}

infixl 6 <+>

(<+>) :: Int -> Int -> Int
(<+>) = blend
