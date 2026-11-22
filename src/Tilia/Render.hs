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
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
  ( ConDecl (..),
    HsConDeclField (..),
    HsConDeclGADTDetails (..),
    HsModule (..),
    HsSigType (..),
    HsType (..),
    LHsConDeclRecField,
    LHsSigType,
    TyClDecl (..),
    XModulePs (..),
  )
import GHC.Hs.Extension (GhcPs)
import GHC.Types.SrcLoc (getLoc, unLoc)

import Tilia.Comments
  ( Comment (..),
    asOrdinary,
    bracketed,
    carriedOnFrom,
    commentRuns,
    commentTrailing,
    continuesHaddock,
    holdsOff,
    sharesItsLine,
    widenTrigger,
  )
import Tilia.Comments.Attach (attachComments)
import Tilia.Doc.Combinators
import Tilia.Fixity (capturedUses)
import Tilia.Gathered (Gathered)
import Tilia.Imports (travellingComments)
import Tilia.MagicComments (preservedLines)
import Tilia.Parser (ParsedModule (..))
import Tilia.Render.Context
import Tilia.Render.Declaration (decls)
import Tilia.Render.Expression (hsExpr, untypedSplice)
import Tilia.Render.Haddock (haddockSpans)
import Tilia.Render.Header
  ( HeaderPragma (..),
    ModuleCtx (..),
    hsModule,
    takeHeaderPragmas,
    takeStackHeader,
  )
import Tilia.Render.Signature (sigDecl)
import Tilia.Render.Type (documentedParts, fieldDoc)
import Tilia.Settings (Settings (..))
import Tilia.Source (Lines, blankAt, comments, lineAt, sourceLines)
import Tilia.Span
import Tilia.Span.Ghc (spanOf, spanOfSrcSpan)

-- | Render a parsed module, comments and all.
renderModule ::
  -- | How to render it.
  RenderConfig ->
  -- | The module, parsed.
  ParsedModule ->
  Doc
renderModule render parsed = attachComments loose doc
  where
    (doc, loose) = renderConfiguration render parsed

-- | Render one configuration of a module, and return the comments the
-- syntax tree does not carry rather than attach them.
renderConfiguration ::
  -- | How to render it.
  RenderConfig ->
  -- | The configuration, parsed.
  ParsedModule ->
  (Doc, [Comment])
renderConfiguration render parsed =
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
      heldOff (documentedBelow (rcSettings render) hsMod) haddocks loose'
    (stackHeader, rest) = takeStackHeader (pmPrologue parsed) plain
    (pragmas, uncovered) =
      takeHeaderPragmas (pmSource parsed) (pmHeaderEnd parsed) rest
    held = heldOffModuleDoc hsMod haddocks pragmas uncovered
    (mctx, loose) = moduleCtx render parsed held
    carryingOn = carriedOnFrom loose
    ctx =
      Ctx
        { ctxExtensions = rcExtensions render,
          ctxSourceType = pmSourceType parsed,
          ctxScope = rcScope render,
          ctxCaptured = capturedUses (pmGathered parsed),
          ctxSource = pmSource parsed,
          ctxLineComments = indexOn (filter (not . sharesItsLine) loose),
          ctxCarryingOn =
            Map.fromList
              [ (startPoint (commentSpan c), remark)
              | c <- loose,
                Just (_, remark) <- [carryingOn c]
              ],
          ctxHaddocks = indexOn haddocks,
          ctxKnot = knot,
          ctxIndentation = settingsIndentation (rcSettings render),
          ctxCommaStyle = settingsCommaStyle (rcSettings render),
          ctxArrowStyle = settingsArrowStyle (rcSettings render),
          ctxMacros = rcMacros render,
          ctxMacroArguments = rcMacroArguments render
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
moduleCtx render parsed held =
  ( ModuleCtx
      { mcRender = render,
        mcOpening = opening,
        mcHeadings = Set.toList (Set.fromList (mapMaybe heading held)),
        mcUnderImports = maybe id IntMap.delete final underImports,
        mcBelowImports = (`IntMap.lookup` underImports) =<< final,
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
    opening = importsOpening ls held (fst <$> bounds (listToMaybe starts))
    imports = mapMaybe spanOf (hsmodImports hsMod)
    among c = amongImports (bounds (Just (line c))) imports c
    final = snd <$> (bounds . Just . NE.last =<< NE.nonEmpty starts)
    bounds having = rcImportBounds render having <|> own
    own = (\ss -> (NE.head ss, NE.last ss)) <$> NE.nonEmpty starts
    starts = fmap spanStartLine imports
    blocks =
      commentRuns (filter (\c -> among c && not (commentTrailing c)) held)
    heading c = do
      guard (among c)
      (top, bottom) <- IntMap.lookup (spanStartLine (commentSpan c)) blocks
      let edges =
            ( spanStartLine (commentSpan top) - 1,
              spanEndLine (commentSpan bottom) + 1
            )
      guard $
        headsImports
          (rcImportBarriers render)
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
  -- | The last line of each part that has its Haddock moved under it,
  -- with the first line of that Haddock as written.
  [(Int, Int)] ->
  -- | The Haddocks, as written.
  [Comment] ->
  -- | The comments.
  [Comment] ->
  [Comment]
heldOff movedUnder haddocks = fmap holdOff
  where
    written = filter holdsOff haddocks
    ends =
      Set.fromList
        (fmap fst movedUnder <> fmap (spanEndLine . commentSpan) written)
    starts =
      Set.fromList
        [ spanStartLine (commentSpan h)
        | h <- written,
          not (commentTrailing h)
        ]
        `Set.difference` Set.fromList (fmap snd movedUnder)
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

-- | The last line of what has its Haddock put under it, with the first
-- line of that Haddock as written: the record fields of leading commas,
-- and the arguments of leading arrows, those of a type synonym among them.
documentedBelow ::
  -- | What the project sets.
  Settings ->
  -- | The module.
  HsModule GhcPs ->
  [(Int, Int)]
documentedBelow settings hsMod = fields <> arguments
  where
    fields = case settingsCommaStyle settings of
      TrailingCommas -> []
      LeadingCommas ->
        [ (spanEndLine s, spanStartLine d)
        | field <- listify documentedField hsMod,
          Just s <- [spanOf field],
          Just d <- [spanOf =<< fieldDoc (unLoc field)]
        ]
    arguments = case settingsArrowStyle settings of
      TrailingArrows -> []
      LeadingArrows ->
        [ (spanEndLine s, spanStartLine d)
        | (part, doc) <- documented,
          Just s <- [spanOf part],
          Just d <- [spanOf doc]
        ]
    documented =
      foldMap
        (foldMap withDoc . documentedParts . sig_body . unLoc)
        (listify (const True :: LHsSigType GhcPs -> Bool) hsMod)
        <> foldMap gadtParts (listify (const True :: ConDecl GhcPs -> Bool) hsMod)
        <> foldMap
          synonymParts
          (listify (const True :: TyClDecl GhcPs -> Bool) hsMod)
    withDoc part = case unLoc part of
      HsDocTy _ _ doc -> [(part, doc)]
      _ -> []
    gadtParts con = case con of
      ConDeclGADT{con_g_args, con_res_ty} ->
        [ (cdf_type x, doc)
        | PrefixConGADT _ xs <- [con_g_args],
          x <- xs,
          Just doc <- [cdf_doc x]
        ]
          <> foldMap withDoc (documentedParts con_res_ty)
      _ -> []
    synonymParts decl = case decl of
      SynDecl{tcdRhs} -> case unLoc tcdRhs of
        HsDocTy{} -> []
        _ -> foldMap withDoc (documentedParts tcdRhs)
      _ -> []
    documentedField :: LHsConDeclRecField GhcPs -> Bool
    documentedField = isJust . fieldDoc . unLoc

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
  -- | The line the first import begins on in the configurations that have
  -- the line of this one's first import, or in any where it has none.
  Maybe Int ->
  Maybe Int
importsOpening ls cs first = climb . pred =<< first
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

-- | Is a comment written between imports rather than inside one?
amongImports ::
  -- | The first and the last line an import begins on in any configuration
  -- that has the comment, where they have imports.
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
    { knotExpr = hsExpr,
      knotSplice = untypedSplice,
      knotSig = sigDecl,
      knotDecls = decls
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
