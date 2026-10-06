-- | Turning a parsed module into a document.
module Tilia.Render
  ( RenderConfig (..),
    defaultRenderConfig,
    renderModule,
    renderConfiguration,
  )
where

import Data.Choice (fromBool)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Hs (HsModule (..), XModulePs (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.SrcLoc (getLoc)
import Tilia.Comments
  ( Comment (..),
    asOrdinary,
    bracketed,
    carriedOnFrom,
    closesItself,
    commentTrailing,
    holdsOff,
    widenTrigger,
  )
import Tilia.Comments.Attach (attachComments)
import Tilia.Doc.Combinators
import Tilia.Fixity (Scope, capturedUses)
import Tilia.Gathered (Gathered)
import Tilia.Imports (normalizeImports)
import Tilia.Parser (ParsedModule (..))
import Tilia.Render.Context
import Tilia.Render.Declaration (decls, declsKeepingGroups)
import Tilia.Render.Expression (hsCmd, hsExprIn, untypedSplice)
import Tilia.Render.Haddock (haddockSpans)
import Tilia.Render.Header (HeaderPragma (..), hsModule, takeHeaderPragmas, takeStackHeader)
import Tilia.Render.Signature (sigDecl)
import Tilia.Source (Lines, blankAt, comments, sourceLines)
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
    rcImportBarriers :: [Int],
    -- | Source lines the names of an import list must not be sorted across.
    rcNameBarriers :: [Int]
  }

-- | A configuration that asserts nothing.
defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    { rcExtensions = Set.empty,
      rcScope = Nothing,
      rcImportBarriers = [],
      rcNameBarriers = []
    }

-- | Render a parsed module, comments and all.
renderModule :: RenderConfig -> ParsedModule -> Doc
renderModule settings parsed = attachComments loose doc
  where
    (doc, loose) = renderConfiguration settings parsed

-- | Render one configuration of a module, and return the comments the
-- syntax tree does not carry rather than attach them.
renderConfiguration :: RenderConfig -> ParsedModule -> (Doc, [Comment])
renderConfiguration settings parsed =
  ( prologue (pmPrologue parsed)
      <> stackHeader
      <> hsModule ctx pragmas opening (sorted hsMod),
    loose
  )
  where
    hsMod = pmModule parsed
    (haddocks, loose') =
      splitHaddocks
        (sourceLines (pmSource parsed))
        (pmGathered parsed)
        (comments (pmSource parsed))
    plain = heldOff haddocks loose'
    (stackHeader, rest) = takeStackHeader (pmHeaderEnd parsed) plain
    (pragmas, uncovered) = takeHeaderPragmas (pmSource parsed) (pmHeaderEnd parsed) rest
    held = heldOffModuleDoc hsMod haddocks pragmas uncovered
    opening = importsOpening (sourceLines (pmSource parsed)) held hsMod
    loose = fmap (belowOpening opening) held
    implicitPrelude =
      fromBool (Set.member ImplicitPrelude (rcExtensions settings))
    sorted m =
      m
        { hsmodImports =
            normalizeImports
              implicitPrelude
              (rcImportBarriers settings)
              (rcNameBarriers settings)
              (comments (pmSource parsed))
              (hsmodImports m)
        }
    ctx =
      Ctx
        { ctxExtensions = rcExtensions settings,
          ctxSourceType = pmSourceType parsed,
          ctxScope = rcScope settings,
          ctxCaptured = capturedUses (pmGathered parsed),
          ctxSource = pmSource parsed,
          ctxLineComments = indexOn (filter (not . closesItself) loose),
          ctxCarryingOn =
            Set.fromList
              (startPoint . commentSpan <$> filter (isJust . carriedOnFrom loose) loose),
          ctxHaddocks = indexOn haddocks,
          ctxKnot = knot
        }

-- | Keep a comment from running into a Haddock.
heldOff :: [Comment] -> [Comment] -> [Comment]
heldOff haddocks = fmap holdOff
  where
    written = filter holdsOff haddocks
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

-- | The empty line right above the first import, or above the comments
-- written on top of it.
importsOpening :: Lines -> [Comment] -> HsModule GhcPs -> Maybe Int
importsOpening ls cs hsMod =
  climb . pred . spanStartLine =<< spanOf =<< listToMaybe (hsmodImports hsMod)
  where
    climb n
      | blankAt n ls = Just n
      | otherwise = climb . pred =<< IntMap.lookup n onTheirOwnLines
    onTheirOwnLines =
      IntMap.fromList
        [ (spanEndLine s, spanStartLine s)
        | c <- cs,
          not (commentTrailing c),
          let s = commentSpan c
        ]

-- | Leave the empty line above the comments written on top of the first
-- import at the top of the imports, rather than carry it along when sorting
-- moves that import.
belowOpening :: Maybe Int -> Comment -> Comment
belowOpening opening c
  | Just l <- opening,
    spanStartLine (commentSpan c) == l + 1 =
      c{commentGapAbove = False}
  | otherwise = c

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
  -- | The module's lines.
  Lines ->
  -- | What the tree holds.
  Gathered ->
  -- | Every comment in the module.
  [Comment] ->
  -- | The ones the tree carries, and the ones it does not.
  ([Comment], [Comment])
splitHaddocks ls found = foldr sort' ([], [])
  where
    inTree = Set.fromList (fmap startPoint (haddockSpans found))
    sort' c (docs, rest)
      | startPoint (commentSpan c) `Set.member` inTree =
          (widenTrigger c : docs, rest)
      | otherwise = (docs, asOrdinary ls c <> rest)

-- | Index comments by where they begin.
indexOn :: [Comment] -> Map (Int, Int) Comment
indexOn cs = Map.fromList [(startPoint (commentSpan c), c) | c <- cs]
