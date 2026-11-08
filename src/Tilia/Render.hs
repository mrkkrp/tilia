-- | Turning a parsed module into a document.
module Tilia.Render
  ( RenderConfig (..),
    defaultRenderConfig,
    renderModule,
    renderConfiguration,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Generics.Schemes (listify)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
  ( ConDecl (..),
    HsConDeclField (..),
    HsConDeclGADTDetails (..),
    HsConDeclRecField (..),
    HsModule (..),
    HsSigType (..),
    LHsConDeclRecField,
    LHsSigType,
    XModulePs (..),
  )
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.SrcLoc (getLoc, unLoc)

import Tilia.Comments
  ( Comment (..),
    asOrdinary,
    bracketed,
    carriedOnFrom,
    closesItself,
    commentTrailing,
    continuesHaddock,
    holdsOff,
    widenTrigger,
  )
import Tilia.Comments.Attach (attachComments)
import Tilia.Doc.Combinators
import Tilia.Fixity (Scope, capturedUses)
import Tilia.Gathered (Gathered)
import Tilia.Imports (travellingComments)
import Tilia.MagicComments (preservedLines)
import Tilia.Parser (ParsedModule (..))
import Tilia.Render.Context
import Tilia.Render.Declaration (decls, declsKeepingGroups)
import Tilia.Render.Expression (hsExprIn, untypedSplice)
import Tilia.Render.Haddock (haddockSpans)
import Tilia.Render.Header
  ( HeaderPragma (..),
    ModuleCtx (..),
    hsModule,
    takeHeaderPragmas,
    takeStackHeader,
  )
import Tilia.Render.Signature (sigDecl)
import Tilia.Render.Type (documentedParts)
import Tilia.Settings (Settings (..), defaultSettings)
import Tilia.Source (Lines, blankAt, comments, lineAt, sourceLines)
import Tilia.Span
import Tilia.Span.Ghc (spanOf, spanOfSrcSpan)

-- | What the printer needs to know about the module beyond its text.
data RenderConfig = RenderConfig
  { -- | Extensions in force, from the module's own pragmas and from the
    -- package it belongs to.
    rcExtensions :: Set Extension,
    -- | What the module can see, if it could be worked out.
    rcScope :: Maybe Scope,
    -- | The lines of every conditional's directives, which the names of an
    -- import list are not sorted across.
    rcDirectiveLines :: [Int],
    -- | The lines of the directives that imports are not sorted across:
    -- those of every conditional but one that only continues the import
    -- above it, such as a @hiding@ clause written behind a condition.
    rcImportBarriers :: [Int],
    -- | The lines the first and the last import of any configuration begin
    -- on, where the module has conditionals, so that every configuration
    -- agrees on which comments are among the imports.
    rcImportBounds :: Maybe (Int, Int),
    -- | The modules the project's own packages declare, each with the names
    -- of those that declare it.
    rcOwnModules :: Map Text (Set Text),
    -- | What the project sets.
    rcSettings :: Settings
  }

-- | A configuration that asserts nothing.
defaultRenderConfig :: RenderConfig
defaultRenderConfig =
  RenderConfig
    { rcExtensions = Set.empty,
      rcScope = Nothing,
      rcDirectiveLines = [],
      rcImportBarriers = [],
      rcImportBounds = Nothing,
      rcOwnModules = Map.empty,
      rcSettings = defaultSettings
    }

-- | Render a parsed module, comments and all.
renderModule :: RenderConfig -> ParsedModule -> Doc
renderModule settings parsed = attachComments loose doc
  where
    (doc, loose) = renderConfiguration settings parsed

-- | Render one configuration of a module, and return the comments the
-- syntax tree does not carry rather than attach them.
renderConfiguration ::
  -- | How to render it.
  RenderConfig ->
  -- | The configuration, parsed.
  ParsedModule ->
  (Doc, [Comment])
renderConfiguration settings parsed =
  ( prologue (pmPrologue parsed)
      <> stackHeader
      <> hsModule ctx mctx pragmas hsMod,
    loose
  )
  where
    hsMod = pmModule parsed
    (haddocks, loose') =
      splitHaddocks
        (sourceLines (pmSource parsed))
        (pmGathered parsed)
        (comments (pmSource parsed))
    plain =
      heldOff (documentedBelow (rcSettings settings) hsMod) haddocks loose'
    (stackHeader, rest) = takeStackHeader (pmPrologue parsed) plain
    (pragmas, uncovered) =
      takeHeaderPragmas (pmSource parsed) (pmHeaderEnd parsed) rest
    held = heldOffModuleDoc hsMod haddocks pragmas uncovered
    (mctx, loose) = moduleCtx settings parsed held
    carryingOn = carriedOnFrom loose
    ctx =
      Ctx
        { ctxExtensions = rcExtensions settings,
          ctxSourceType = pmSourceType parsed,
          ctxScope = rcScope settings,
          ctxCaptured = capturedUses (pmGathered parsed),
          ctxSource = pmSource parsed,
          ctxLineComments = indexOn (filter (not . closesItself) loose),
          ctxCarryingOn =
            Map.fromList
              [ (startPoint (commentSpan c), remark)
              | c <- loose,
                Just (_, remark) <- [carryingOn c]
              ],
          ctxHaddocks = indexOn haddocks,
          ctxKnot = knot,
          ctxCommaStyle = settingsCommaStyle (rcSettings settings),
          ctxArrowStyle = settingsArrowStyle (rcSettings settings)
        }

-- | What printing the top level of one configuration takes, and the
-- comments it does not carry with the empty lines around them that stay.
moduleCtx ::
  -- | How to render it.
  RenderConfig ->
  -- | The configuration, parsed.
  ParsedModule ->
  -- | The comments neither its syntax tree nor its header carries.
  [Comment] ->
  (ModuleCtx, [Comment])
moduleCtx settings parsed held =
  ( ModuleCtx
      { mcOpening = opening,
        mcHeadings = Set.toList (Set.fromList (mapMaybe heading held)),
        mcUnderImports = maybe id (IntMap.delete . snd) bounds underImports,
        mcBelowImports = (`IntMap.lookup` underImports) . snd =<< bounds,
        mcDirectiveLines = rcDirectiveLines settings,
        mcImportBarriers = rcImportBarriers settings,
        mcOwnModules = rcOwnModules settings,
        mcMagicCommentRanges = magicCommentRanges
      },
    [ if among c && isNothing (heading c) || IntSet.member (line c) travellers
        then withoutGaps c'
        else c'
    | c <- held,
      not (leftAlone c),
      let c' = belowOpening opening c
    ]
  )
  where
    hsMod = pmModule parsed
    ls = sourceLines (pmSource parsed)
    opening = importsOpening ls held hsMod
    imports = mapMaybe spanOf (hsmodImports hsMod)
    among = amongImports bounds imports
    bounds =
      rcImportBounds settings
        <|> fmap
          (\starts -> (NE.head starts, NE.last starts))
          (NE.nonEmpty (fmap spanStartLine imports))
    blocks = blockEdges (filter among held)
    heading c = do
      guard (among c)
      edges <- IntMap.lookup (spanStartLine (commentSpan c)) blocks
      guard $
        headsImports
          (rcImportBarriers settings)
          ls
          imports
          (spanStartColumn (commentSpan c))
          edges
      pure (snd edges)
    travelling = travellingComments ls held hsMod
    underImports = fmap ((+ 1) . spanEndLine . commentSpan . NE.last) travelling
    travellers = IntSet.fromList (line <$> foldMap NE.toList travelling)
    line = spanStartLine . commentSpan
    withoutGaps c = c{commentGapAbove = False, commentGapBelow = False}
    leftAlone c = any (holds (spanStartLine (commentSpan c))) magicCommentRanges
    holds n (from, to) = from <= n && n <= to
    magicCommentRanges = preservedLines (pmSource parsed)

-- | Keep a comment from running into a Haddock.
heldOff ::
  -- | The last lines of what has its Haddock moved under it.
  [Int] ->
  -- | The Haddocks, as written.
  [Comment] ->
  -- | The comments.
  [Comment] ->
  [Comment]
heldOff movedUnder haddocks = fmap holdOff
  where
    written = filter holdsOff haddocks
    ends =
      Set.fromList (movedUnder <> fmap (spanEndLine . commentSpan) written)
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
                commentGapAbove c
                  || continuesHaddock c
                    && Set.member (spanStartLine s - 1) ends,
              commentGapBelow =
                commentGapBelow c || Set.member (spanEndLine s + 1) starts
            }
      where
        s = commentSpan c

-- | The last lines of what has its Haddock put under it: the record fields
-- of leading commas, and the arguments of leading arrows.
documentedBelow ::
  -- | What the project sets.
  Settings ->
  -- | The module.
  HsModule GhcPs ->
  [Int]
documentedBelow settings hsMod = fields <> arguments
  where
    fields = case settingsCommaStyle settings of
      TrailingCommas -> []
      LeadingCommas ->
        [ spanEndLine s
        | field <- listify documentedField hsMod,
          Just s <- [spanOf field]
        ]
    arguments = case settingsArrowStyle settings of
      TrailingArrows -> []
      LeadingArrows ->
        mapMaybe (fmap spanEndLine . spanOf) $
          foldMap
            (documentedParts . sig_body . unLoc)
            (listify (const True :: LHsSigType GhcPs -> Bool) hsMod)
            <> foldMap
              gadtParts
              (listify (const True :: ConDecl GhcPs -> Bool) hsMod)
    gadtParts con = case con of
      ConDeclGADT{con_g_args, con_res_ty} ->
        [ cdf_type x
        | PrefixConGADT _ xs <- [con_g_args],
          x <- xs,
          isJust (cdf_doc x)
        ]
          <> documentedParts con_res_ty
      _ -> []
    documentedField :: LHsConDeclRecField GhcPs -> Bool
    documentedField = isJust . cdf_doc . cdrf_spec . unLoc

-- | Hold the comment that comes out right under the module's own Haddock
-- off it: the first one between the Haddock and the module line with no
-- pragma after it, since one with a pragma after it is placed with that
-- pragma, above the Haddock.
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
        && not (any (\p -> spanEndLine here < p && p < began) pragmaStarts)
      where
        here = commentSpan c
    pragmaStarts = fmap (spanStartLine . hpSpan) pragmas
    endOfModuleDoc = do
      s <- moduleDoc
      c <-
        lookup (startPoint s) [(startPoint (commentSpan h), h) | h <- haddocks]
      if bracketed c then Nothing else Just (spanEndLine s)
    moduleDoc = case hsmodExt hsMod of
      XModulePs{hsmodHaddockModHeader = Just d} -> spanOfSrcSpan (getLoc d)
      _ -> Nothing
    startOfModuleLine = spanStartLine <$> (spanOf =<< hsmodName hsMod)

-- | The empty line right above the first import, or above the comments
-- written on top of it and the lines that hold only semicolons.
importsOpening ::
  -- | The module as written.
  Lines ->
  -- | Its comments.
  [Comment] ->
  -- | The module.
  HsModule GhcPs ->
  Maybe Int
importsOpening ls cs hsMod =
  climb . pred . spanStartLine =<< spanOf =<< listToMaybe (hsmodImports hsMod)
  where
    climb n
      | blankAt n ls = Just n
      | maybe False (T.all (\ch -> ch == ';' || isSpace ch)) (lineAt n ls) =
          climb (pred n)
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

-- | Does a block of comments head the imports under it: is it written at
-- their indentation, with empty lines right above and right under it, and
-- no line they must not be sorted across between it and the first of them?
headsImports ::
  -- | The lines the imports are not sorted across.
  [Int] ->
  -- | The module as written.
  Lines ->
  -- | Where the imports are.
  [Span] ->
  -- | The column the block begins at.
  Int ->
  -- | The lines right above and right under the block.
  (Int, Int) ->
  Bool
headsImports barriers ls imports column (above, under) =
  blankAt above ls
    && blankAt under ls
    && case dropWhile ((< under) . spanStartLine) imports of
      next : _ ->
        spanStartColumn next == column
          && not (any (\b -> under < b && b < spanStartLine next) barriers)
      [] -> False

-- | The lines right above and right under each block of comments on lines
-- of their own, written one right under another at one column, by the line
-- each comment of the block begins on.
blockEdges :: [Comment] -> IntMap (Int, Int)
blockEdges cs =
  IntMap.fromList
    [ (spanStartLine (commentSpan c), (above, under))
    | block <- foldr joined [] (filter (not . commentTrailing) cs),
      let above = spanStartLine (commentSpan (NE.head block)) - 1
          under = spanEndLine (commentSpan (NE.last block)) + 1,
      c <- NE.toList block
    ]
  where
    joined c ((d :| ds) : rest)
      | continuedBy c d = (c :| d : ds) : rest
    joined c rest = (c :| []) : rest
    continuedBy c d =
      spanStartLine (commentSpan d) == spanEndLine (commentSpan c) + 1
        && spanStartColumn (commentSpan d) == spanStartColumn (commentSpan c)

-- | Is a comment written between imports rather than inside one?
amongImports ::
  -- | The first and the last line an import begins on in any configuration
  -- of the module, where it has imports.
  Maybe (Int, Int) ->
  -- | Where the imports of this configuration are.
  [Span] ->
  -- | The comment.
  Comment ->
  Bool
amongImports bounds imports c = case bounds of
  Just (first, final) ->
    (first < spanStartLine s || first == spanStartLine s && commentTrailing c)
      && spanEndLine s < final
      && not (any (`covers` s) imports)
  Nothing -> False
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
