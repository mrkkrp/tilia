-- | The nodes of a module that formatting queries, gathered in one walk
-- over its syntax tree.
module Tilia.Gathered
  ( Gathered (..),
    gathered,
  )
where

import Data.Data (gmapQr)
import Data.Generics.Aliases (GenericQ, extQ)
import GHC.Hs

-- | Every node of each kind, in the order a walk from the top, left to
-- right, meets them.
data Gathered = Gathered
  { -- | The comments the annotations hold.
    gatheredComments :: [EpAnnComments],
    -- | Expressions.
    gatheredExpressions :: [HsExpr GhcPs],
    -- | Types.
    gatheredTypes :: [HsType GhcPs],
    -- | Haddocks.
    gatheredDocs :: [LHsDoc GhcPs],
    -- | The entries of import and export lists.
    gatheredEntries :: [LIE GhcPs]
  }

-- | Gather a module's nodes.
gathered :: HsModule GhcPs -> Gathered
gathered hsModule = walk hsModule (Gathered [] [] [] [] [])
  where
    walk :: GenericQ (Gathered -> Gathered)
    walk x = keep x . descend x
    descend :: GenericQ (Gathered -> Gathered)
    descend =
      (\x g -> gmapQr ($) g walk x)
        `extQ` (\(_ :: HsDocString) -> id)
        `extQ` (\(_ :: String) -> id)
    keep :: GenericQ (Gathered -> Gathered)
    keep =
      const id
        `extQ` (\x (Gathered cs es ts ds ls) -> Gathered (x : cs) es ts ds ls)
        `extQ` (\x (Gathered cs es ts ds ls) -> Gathered cs (x : es) ts ds ls)
        `extQ` (\x (Gathered cs es ts ds ls) -> Gathered cs es (x : ts) ds ls)
        `extQ` (\x (Gathered cs es ts ds ls) -> Gathered cs es ts (x : ds) ls)
        `extQ` (\x (Gathered cs es ts ds ls) -> Gathered cs es ts ds (x : ls))
