-- | Something that can appear as the body of an enclosing construct.
module Tilia.Doc.Body
  ( Body (..),
    attachBody,
  )
where

import Tilia.Doc.Combinators

-- | Something that can appear as the body of an enclosing construct.
class Body a where
  -- | Print it.
  printBody :: a -> Doc

  -- | Whether it absorbs its own line break.
  bodyPlacement :: a -> Placement

-- | Print a body and join it to whatever precedes it.
attachBody :: (Body a) => a -> Doc
attachBody x = attach (bodyPlacement x) (printBody x)
