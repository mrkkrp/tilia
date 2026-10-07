-- | What the syntax walk needs to know that the syntax tree does not say.
module Tilia.Render.Context
  ( -- * The context
    Ctx (..),
    Knot (..),
    FamilyStyle (..),

    -- * Sites
    Site (..),
    plainSite,
    withBracing,
    underSite,
    closingFor,

    -- * Extensions
    extensionOn,

    -- * Fixities
    operatorFixity,

    -- * What lies between two spans
    commentPrintedBetween,
    lineCommentWrittenBetween,
    commentRightUnder,
    remarkUnder,
    separatedByBlank,

    -- * Entering the tree
    at,
    at_,
    atSpan,
    keywordAt,
    fenceWithin,
    layoutFrom,
    layoutWithin,
    layoutAcross,
    attachOperator,

    -- * Haddocks
    writtenHaddock,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs hiding (Fixity)
import GHC.LanguageExtensions.Type (Extension)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..))
import GHC.Types.SrcLoc qualified as GHC
import Tilia.Comments (Comment (..), CommentStyle (..), commentTrailing)
import Tilia.Doc.Combinators
import Tilia.Fixity
  ( Fixity,
    Namespace (..),
    OpName (..),
    Resolution (..),
    Scope,
    lookupFixity,
  )
import Tilia.Render.Layout (Bracing (..))
import Tilia.Source
  ( Source,
    SourceType,
    blankAt,
    directivePresentOnLine,
    sourceLines,
  )
import Tilia.Span
import Tilia.Span.Ghc

----------------------------------------------------------------------------
-- The context

-- | Everything a printer may need beyond the node it is given.
data Ctx = Ctx
  { -- | Extensions in force.
    ctxExtensions :: Set Extension,
    -- | The type of the source file.
    ctxSourceType :: SourceType,
    -- | What imports the module can see, if that could be worked out.
    ctxScope :: Maybe Scope,
    -- | The uses of an operator a local binding captures, by where the
    -- operator is written, with their fixities.
    ctxCaptured :: Map Span Fixity,
    -- | The comments that take whole lines, by starting position.
    ctxLineComments :: Map (Int, Int) Comment,
    -- | Where each comment that carries on a trailing comment begins.
    ctxCarryingOn :: Set (Int, Int),
    -- | The module as its author wrote it.
    ctxSource :: Source,
    -- | The author's own text for each Haddock, by starting position.
    ctxHaddocks :: Map (Int, Int) Comment,
    -- | The knot.
    ctxKnot :: Knot
  }

-- | The backward edges of the rendering knot.
data Knot = Knot
  { -- | Expressions, needed by types, patterns and bodies.
    knotExpr :: Ctx -> Site -> LHsExpr GhcPs -> Doc,
    -- | Commands, needed by bodies.
    knotCmd :: Ctx -> Site -> LHsCmd GhcPs -> Doc,
    -- | Splices, needed by types and patterns, defined with expressions.
    knotSplice :: Ctx -> SpliceDecoration -> HsUntypedSplice GhcPs -> Doc,
    -- | Signature declarations, needed by @let@ and @where@ bodies.
    knotSig :: Ctx -> Sig GhcPs -> Doc,
    -- | A run of declarations, needed by Template Haskell brackets.
    knotDecls :: Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc,
    -- | A run of declarations that keeps the author's blank lines, needed
    -- by class and instance bodies.
    knotDeclsGrouped :: Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc
  }

-- | Whether a family or data declaration stands on its own or inside a
-- class.
data FamilyStyle
  = Associated
  | Free
  deriving (Eq, Show)

----------------------------------------------------------------------------
-- Sites

-- | What the surroundings of a node oblige it to do.
data Site = Site
  { -- | Is this the function of an application, as @f@ is in @f a@?
    siteApplicand :: Bool,
    -- | Is this an item of a layout block?
    --
    -- Such an item may not leave a bracket open for the block's own layout
    -- to close, so a list comprehension standing as a statement is arranged
    -- differently from one standing anywhere else.
    siteInBlock :: Bool,
    -- | May a block inside this node put braces round itself?
    siteBracing :: Bracing
  }
  deriving (Eq, Show)

-- | A node standing on its own.
plainSite :: Site
plainSite =
  Site
    { siteApplicand = False,
      siteInBlock = False,
      siteBracing = NoBrace
    }

-- | The same site, with a different answer about braces.
withBracing :: Bracing -> Site -> Site
withBracing bracing site = site{siteBracing = bracing}

-- | Indent a hanging body, one step further when it hangs off an applicand.
underSite :: Site -> Doc -> Doc
underSite site =
  nest (if siteApplicand site then 2 else 1)

-- | Where a bracket opened here has to close.
closingFor :: Site -> ClosingIndent
closingFor site = if siteInBlock site then Indented else Outdented

----------------------------------------------------------------------------
-- Extensions

-- | Is the extension on?
extensionOn :: Ctx -> Extension -> Bool
extensionOn ctx e = Set.member e (ctxExtensions ctx)

----------------------------------------------------------------------------
-- Fixities

-- | The fixity of an operator, if one was established.
operatorFixity :: Ctx -> Namespace -> RdrName -> Maybe Fixity
operatorFixity ctx namespace name = do
  scope <- ctxScope ctx
  case lookupFixity scope namespace qualifier op of
    Resolved fixity _ -> Just fixity
    Unresolved _ -> Nothing
  where
    op = OpName (T.pack (occNameString (rdrNameOcc name)))
    qualifier = case name of
      Qual m _ -> Just (T.pack (moduleNameString m))
      _ -> Nothing

----------------------------------------------------------------------------
-- What lies between two spans

-- | Where the next thing to be printed begins. It is either the third
-- argument or a comment, if there is any between the two spans.
nextPrinted ::
  -- | The context.
  Ctx ->
  -- | What has just been printed.
  Maybe Span ->
  -- | What follows it, if nothing comes between.
  Maybe Span ->
  Maybe Span
nextPrinted ctx (Just a) mb@(Just b) =
  case printedBetween ctx a b of
    (c : _) -> Just (commentSpan c)
    [] -> mb
nextPrinted _ _ mb = mb

-- | The comments printed between two spans rather than after the first.
printedBetween :: Ctx -> Span -> Span -> [Comment]
printedBetween ctx a b = filter (not . printedAfter) (Map.elems inTheGap)
  where
    inTheGap =
      Map.takeWhileAntitone (< startPoint b) $
        Map.dropWhileAntitone (< endPoint a) (ctxLineComments ctx)
    printedAfter c =
      commentTrailing c
        || ( Set.member (startPoint s) (ctxCarryingOn ctx)
               && spanStartColumn b < spanStartColumn s
           )
      where
        s = commentSpan c

-- | Is a comment going to be printed between the two spans?
commentPrintedBetween :: Ctx -> Maybe Span -> Maybe Span -> Bool
commentPrintedBetween ctx a b = nextPrinted ctx a b /= b

-- | Was a comment that ends its line written between the two spans, wherever
-- it is printed?
lineCommentWrittenBetween :: Ctx -> Maybe Span -> Maybe Span -> Bool
lineCommentWrittenBetween ctx (Just a) (Just b) =
  holdsLineComment ctx (mkSpan (endPoint a) (startPoint b))
lineCommentWrittenBetween _ _ _ = False

-- | Do the comments printed between the two spans begin right under the
-- first, with an empty line the author left under one of them and no
-- preprocessor directive anywhere between?
remarkUnder :: Ctx -> Maybe Span -> Maybe Span -> Bool
remarkUnder ctx ma@(Just a) mb@(Just b) =
  not (separatedByBlank ctx ma mb)
    && not (any directiveAt [spanEndLine a + 1 .. spanStartLine b - 1])
    && any
      (writtenBlank ctx . (+ 1) . spanEndLine . commentSpan)
      (printedBetween ctx a b)
  where
    directiveAt n = directivePresentOnLine n (sourceLines (ctxSource ctx))
remarkUnder _ _ _ = False

-- | Does the first comment printed between the two spans begin on the line
-- right under the first?
commentRightUnder :: Ctx -> Maybe Span -> Maybe Span -> Bool
commentRightUnder ctx (Just a) (Just b) = case printedBetween ctx a b of
  c : _ -> spanStartLine (commentSpan c) == spanEndLine a + 1
  [] -> False
commentRightUnder _ _ _ = False

-- | Did the author leave an empty line directly after the first of these?
separatedByBlank :: Ctx -> Maybe Span -> Maybe Span -> Bool
separatedByBlank ctx ma@(Just a) mb = case nextPrinted ctx ma mb of
  Just s -> any (writtenBlank ctx) [spanEndLine a + 1 .. spanStartLine s - 1]
  Nothing -> False
separatedByBlank _ _ _ = False

-- | Did the author leave this line empty?
--
-- Asked of the module as written, so that a line the preprocessor support
-- emptied to make one configuration does not read as one the author left
-- blank.
writtenBlank :: Ctx -> Int -> Bool
writtenBlank ctx n = blankAt n (sourceLines (ctxSource ctx))

----------------------------------------------------------------------------
-- Entering the tree

-- | Enter a located node.
--
-- This is the counterpart of every @L@ in the syntax tree: it records where
-- the output came from, so that comments can be attached to it later, and
-- it settles the node's layout from the region it occupied.
at :: (HasLoc l) => Ctx -> GenLocated l a -> (a -> Doc) -> Doc
at ctx l f = atSpan ctx (spanOf l) (f (GHC.unLoc l))

-- | 'at' with the arguments the other way round, for use in sections.
at_ :: (HasLoc l) => Ctx -> (a -> Doc) -> GenLocated l a -> Doc
at_ ctx f l = at ctx l f

-- | Lay a region out as it was written, and claim it.
--
-- Claiming is the difference between this and 'layoutFrom': a comment
-- written anywhere inside the region attaches to this document. So it is
-- for the handful of things a comment can be written against that the
-- syntax tree gives no node for—the @where@ that opens a body, the @then@
-- of an @if@—and for nothing else.
atSpan :: Ctx -> Maybe Span -> Doc -> Doc
atSpan _ Nothing d = d
atSpan _ (Just s) d = located s (group s d)

-- | A keyword, claiming the span it was written on.
keywordAt :: Ctx -> Maybe Span -> Text -> Doc
keywordAt ctx s = atSpan ctx s . txt

-- | Prevent comments inside the given region to float out of it and attach
-- to elements outside.
fenceWithin :: Ctx -> Maybe Span -> Doc -> Doc
fenceWithin _ Nothing d = d
fenceWithin _ (Just s) d = fence s d

-- | Lay a region out as it was written, and claim nothing.
--
-- The region decides one thing—whether what is printed here goes on one
-- line or several.
layoutFrom :: Ctx -> Maybe Span -> Doc -> Doc
layoutFrom _ Nothing d = flat d
layoutFrom _ (Just s) d = group s d

-- | Lay a construct out from the region its contents occupy rather than the
-- region it occupies.
layoutWithin ::
  -- | The context.
  Ctx ->
  -- | The whole construct, delimiters included. Where comments are looked
  -- for.
  Maybe Span ->
  -- | What it holds, delimiters left out.
  Maybe Span ->
  Doc ->
  Doc
layoutWithin ctx whole contents d
  | any (holdsLineComment ctx) whole = broken d
  | otherwise = maybe (flat d) (`group` d) contents

-- | 'layoutFrom' over the region several located things cover.
layoutAcross :: (HasLoc l) => Ctx -> [GenLocated l a] -> Doc -> Doc
layoutAcross ctx xs = layoutFrom ctx (spansOf xs)

-- | 'attach' an operator and its operand, leaving the operator's region at
-- the end of the line when it was written there, so that the comments
-- written around it stay on that line, and giving the indentation of the
-- operand's line a region, so that those above the operand come before it.
attachOperator ::
  Placement ->
  -- | What the operator follows.
  Maybe Span ->
  -- | The operator.
  Maybe Span ->
  -- | Its operand.
  Maybe Span ->
  -- | The operator, printed.
  Doc ->
  -- | Its operand, printed.
  Doc ->
  Doc
attachOperator placement before here after op operand = case here of
  Just s
    | placement == Normal,
      sameLine before here,
      not (sameLine here after) ->
        indent (located s mempty)
          <> attach
            Normal
            (foldMap indentation after <> unclaimed op <> space <> operand)
  _ -> attach placement (op <> space <> operand)
  where
    indentation o =
      located (mkSpan (spanStartLine o, 1) (startPoint o)) mempty

-- | Does a comment that takes whole lines begin inside this span?
holdsLineComment :: Ctx -> Span -> Bool
holdsLineComment ctx s =
  case Map.lookupGE (startPoint s) (ctxLineComments ctx) of
    Just (start, _) -> start < endPoint s
    Nothing -> False

----------------------------------------------------------------------------
-- Haddocks

-- | The author's own text for the Haddock at this position, if we kept it.
writtenHaddock :: Ctx -> Maybe Span -> Maybe (NonEmpty Text)
writtenHaddock ctx ms = do
  s <- ms
  c <- Map.lookup (spanStartLine s, spanStartColumn s) (ctxHaddocks ctx)
  case commentStyle c of
    DocComment -> Just (commentBody c)
    _ -> Nothing
