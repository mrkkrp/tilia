{-# LANGUAGE TypeFamilies #-}

-- A pragma naming an associated type stays next to the class declaring it.
module Archive.Box where

class Boxed b where
  type Content b
  open :: b -> Content b
{-# DEPRECATED Content "Use open instead." #-}
