-- | Turning a parsed module into a document.
module Tilia.Render
  ( RenderConfig (..),
    defaultRenderConfig,
    renderModule,
  )
where

import Data.Choice (fromBool)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Hs (HsModule (..), XModulePs (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.SrcLoc (getLoc)
import Tilia.Comments
  ( Comment (..),
    bracketed,
    closesItself,
    commentTrailing,
    escapeTrigger,
    widenTrigger,
  )
import Tilia.Comments.Attach (attachComments)
import Tilia.Doc.Combinators
import Tilia.Fixity (Scope)
import Tilia.Imports (normalizeImports)
import Tilia.Parser (ParsedModule (..))
import Tilia.Render.Context
import Tilia.Render.Declaration (decls, declsKeepingGroups)
import Tilia.Render.Expression (hsCmd, hsExprIn, untypedSplice)
import Tilia.Render.Haddock (haddockSpans)
import Tilia.Render.Header (HeaderPragma (..), hsModule, takeHeaderPragmas, takeStackHeader)
import Tilia.Render.Signature (sigDecl)
import Tilia.Source (comments)
import Tilia.Span
import Tilia.Span.Ghc (spanOf, spanOfSrcSpan)

-- | What the printer needs to know about the module beyond its text.
data RenderConfig = RenderConfig
  { -- | Extensions in force, from the module's own pragmas and from the
    -- package it belongs to.
    rcExtensions :: Set Extension,
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
    (haddocks, loose') = splitHaddocks hsMod (comments (pmSource parsed))
    plain = heldOff haddocks loose'
    (stackHeader, rest) = takeStackHeader (pmHeaderEnd parsed) plain
    (pragmas, uncovered) = takeHeaderPragmas (pmSource parsed) (pmHeaderEnd parsed) rest
    loose = heldOffModuleDoc hsMod haddocks pragmas uncovered
    implicitPrelude =
      fromBool (Set.member ImplicitPrelude (rcExtensions settings))
    sorted m =
      m
        { hsmodImports =
            normalizeImports
              implicitPrelude
              (rcImportBarriers settings)
              (comments (pmSource parsed))
              (hsmodImports m)
        }
    ctx =
      Ctx
        { ctxExtensions = rcExtensions settings,
          ctxSourceType = pmSourceType parsed,
          ctxScope = rcScope settings,
          ctxSource = pmSource parsed,
          ctxLineComments = indexOn (filter (not . closesItself) loose),
          ctxHaddocks = indexOn haddocks,
          ctxKnot = knot
        }

-- | Keep a comment from running into a Haddock.
heldOff :: [Comment] -> [Comment] -> [Comment]
heldOff haddocks = fmap holdOff
  where
    written = filter (not . bracketed) haddocks
    ends = Set.fromList (fmap (spanEndLine . commentSpan) written)
    starts =
      Set.fromList
        [ spanStartLine (commentSpan h)
        | h <- written,
          not (commentTrailing h)
        ]
    holdOff c
      | bracketed c = c
      | otherwise =
          c
            { commentGapAbove =
                commentGapAbove c || Set.member (spanStartLine s - 1) ends,
              commentGapBelow =
                commentGapBelow c || Set.member (spanEndLine s + 1) starts
            }
      where
        s = commentSpan c

-- | Hold the first comment of the header off the module's own Haddock.
heldOffModuleDoc ::
  HsModule GhcPs ->
  -- | The Haddocks of the module, the module's own among them.
  [Comment] ->
  -- | The pragmas the header is about to hoist.
  [HeaderPragma] ->
  [Comment] ->
  [Comment]
heldOffModuleDoc hsMod haddocks pragmas cs
  | Just ended <- endOfModuleDoc,
    Just began <- startOfModuleLine,
    (before', c : after') <- break (uncoveredBetween ended began) cs =
      before' <> (c{commentGapAbove = True} : after')
  | otherwise = cs
  where
    uncoveredBetween ended began c =
      ended < spanStartLine here
        && spanStartLine here < began
        && not (Set.member (spanEndLine here + 1) travellers)
      where
        here = commentSpan c
    travellers = Set.fromList (fmap (spanStartLine . hpSpan) pragmas)
    endOfModuleDoc = do
      s <- moduleDoc
      c <- lookup (startPoint s) [(startPoint (commentSpan h), h) | h <- haddocks]
      if bracketed c then Nothing else Just (spanEndLine s)
    moduleDoc = case hsmodExt hsMod of
      XModulePs{hsmodHaddockModHeader = Just d} -> spanOfSrcSpan (getLoc d)
      _ -> Nothing
    startOfModuleLine = spanStartLine <$> (spanOf =<< hsmodName hsMod)

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
  -- | Every comment in the module.
  [Comment] ->
  -- | The ones the tree carries, and the ones it does not.
  ([Comment], [Comment])
splitHaddocks hsMod = foldr sort' ([], [])
  where
    inTree = Set.fromList (fmap startPoint (haddockSpans hsMod))
    sort' c (docs, rest)
      | startPoint (commentSpan c) `Set.member` inTree =
          (widenTrigger c : docs, rest)
      | otherwise = (docs, escapeTrigger c : rest)

-- | Index comments by where they begin.
indexOn :: [Comment] -> Map (Int, Int) Comment
indexOn cs = Map.fromList [(startPoint (commentSpan c), c) | c <- cs]
