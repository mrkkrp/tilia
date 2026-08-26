{-# LANGUAGE OverloadedStrings #-}

-- | Turning a parsed module into a document.
module Tilia.Render
  ( RenderConfig (..),
    defaultRenderConfig,
    renderModule,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Hs (HsModule (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension (..))
import Tilia.Comments
  ( Comment (..),
    closesItself,
    documentsNothing,
    escapeTrigger,
    widenTrigger,
  )
import Tilia.Comments.Attach (attachComments)
import Tilia.Fixity (Scope)
import Tilia.Imports (normalizeImports)
import Tilia.Parser (ParsedModule (..))
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Declaration (decls, declsKeepingGroups)
import Tilia.Render.Expression (hsCmd, hsExprIn, untypedSplice)
import Tilia.Render.Haddock (haddockSpans)
import Tilia.Render.Header (hsModule, takeHeaderPragmas, takeStackHeader)
import Tilia.Render.Signature (sigDecl)
import Tilia.Span

-- | What the printer needs to know about the module beyond its text.
data RenderConfig = RenderConfig
  { -- | Extensions in force, from the module's own pragmas and from the
    -- package it belongs to.
    rcExtensions :: Set Extension,
    -- | Whether this is a module or a Backpack signature.
    rcSourceType :: SourceType,
    -- | What the module can see, if it could be worked out.
    rcScope :: Maybe Scope,
    -- | Source lines the import block must not be sorted across.
    rcImportBarriers :: [Int]
  }

-- | A configuration that asserts nothing.
defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    { rcExtensions = Set.empty,
      rcSourceType = ModuleSource,
      rcScope = Nothing,
      rcImportBarriers = []
    }

-- | Render a parsed module, comments and all.
renderModule :: RenderConfig -> ParsedModule -> Doc
renderModule settings parsed =
  prologue (pmPrologue parsed)
    <> stackHeader
    <> attachComments loose (hsModule ctx pragmas (sorted hsMod))
  where
    hsMod = pmModule parsed
    (haddocks, loose') = splitHaddocks hsMod (pmComments parsed)
    plain = heldOff haddocks loose'
    (stackHeader, rest) = takeStackHeader (pmHeaderEnd parsed) plain
    (pragmas, loose) = takeHeaderPragmas (pmHeaderEnd parsed) rest

    sorted m =
      m
        { hsmodImports =
            normalizeImports
              (Set.member ImplicitPrelude (rcExtensions settings))
              (rcImportBarriers settings)
              (hsmodImports m)
        }
    ctx =
      Ctx
        { ctxExtensions = rcExtensions settings,
          ctxSourceType = rcSourceType settings,
          ctxScope = rcScope settings,
          ctxLineComments = indexOn (filter (not . closesItself) loose),
          ctxHaddocks = indexOn haddocks,
          ctxKnot = knot
        }

-- | Keep a comment from running into a Haddock.
heldOff :: [Comment] -> [Comment] -> [Comment]
heldOff haddocks = map holdOff
  where
    ends = Set.fromList (map (spanEndLine . commentSpan) haddocks)
    starts = Set.fromList (map (spanStartLine . commentSpan) haddocks)
    holdOff c =
      c
        { commentGapAbove =
            commentGapAbove c || Set.member (spanStartLine s - 1) ends,
          commentGapBelow =
            commentGapBelow c || Set.member (spanEndLine s + 1) starts
        }
      where
        s = commentSpan c

-- | The lines above the module, put back exactly as they were written.
prologue :: [Text] -> Doc
prologue = foldMap (\l -> txt l <> hardBreak)

-- | The knot: the printers that a module below their definition needs.
knot :: Knot
knot =
  Knot
    { knotExpr = hsExprIn,
      knotCmd = hsCmd,
      knotSplice = untypedSplice,
      knotSig = sigDecl,
      knotDecls = decls,
      knotDeclsGrouped = declsKeepingGroups
    }

-- | Separate the comments the syntax tree also knows about from the rest.
splitHaddocks ::
  HsModule GhcPs ->
  -- | Every comment in the module
  [Comment] ->
  -- | The ones the tree carries, and the ones it does not
  ([Comment], [Comment])
splitHaddocks hsMod = foldr sort' ([], [])
  where
    inTree = Set.fromList (map startPoint (haddockSpans hsMod))
    sort' c (docs, rest)
      | startPoint (commentSpan c) `Set.member` inTree,
        not (documentsNothing c) =
          (widenTrigger c : docs, rest)
      | otherwise = (docs, escapeTrigger c : rest)

-- | Index comments by where they begin.
indexOn :: [Comment] -> Map (Int, Int) Comment
indexOn cs = Map.fromList [(startPoint (commentSpan c), c) | c <- cs]
