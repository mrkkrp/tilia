-- | The nodes of a module that formatting queries, gathered in one walk
-- over its syntax tree.
module Tilia.Gathered
  ( Gathered (..),
    gathered,
  )
where

import Data.Data (gmapQr)
import Data.Generics.Aliases (GenericQ, ext1Q, extQ)
import GHC.Hs

-- | Every node of each kind, in the order a walk from the top, left to
-- right, meets them.
data Gathered = Gathered
  { -- | Expressions.
    gatheredExpressions :: [HsExpr GhcPs],
    -- | Types.
    gatheredTypes :: [HsType GhcPs],
    -- | Haddocks.
    gatheredDocs :: [LHsDoc GhcPs],
    -- | The entries of import and export lists.
    gatheredEntries :: [LIE GhcPs],
    -- | The equations of functions, the alternatives of cases, and the
    -- bodies of lambdas.
    gatheredMatches :: [Match GhcPs (LHsExpr GhcPs)]
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
        `ext1Q` (\(_ :: EpAnn a) -> id)
    keep :: GenericQ (Gathered -> Gathered)
    keep =
      const id
        `extQ` (\x g -> g{gatheredExpressions = x : gatheredExpressions g})
        `extQ` (\x g -> g{gatheredTypes = x : gatheredTypes g})
        `extQ` (\x g -> g{gatheredDocs = x : gatheredDocs g})
        `extQ` (\x g -> g{gatheredEntries = x : gatheredEntries g})
        `extQ` (\x g -> g{gatheredMatches = x : gatheredMatches g})
