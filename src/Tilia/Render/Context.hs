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
    commentBetween,
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
    insideBrackets,

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
import Tilia.Source (Source, SourceType, blankAt, sourceLines)
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
    -- | The comments that take whole lines, by starting position.
    ctxLineComments :: Map (Int, Int) Comment,
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
  case filter (not . commentTrailing) (Map.elems inTheGap) of
    (c : _) -> Just (commentSpan c)
    [] -> mb
  where
    inTheGap =
      Map.takeWhileAntitone (< startPoint b) $
        Map.dropWhileAntitone (< endPoint a) (ctxLineComments ctx)
nextPrinted _ _ mb = mb

-- | Is a comment going to be printed between the two spans?
commentBetween :: Ctx -> Maybe Span -> Maybe Span -> Bool
commentBetween ctx a b = nextPrinted ctx a b /= b

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
atSpan ctx (Just s) d = located s (grouped ctx s d)

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
layoutFrom ctx (Just s) d = grouped ctx s d

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

-- | Give the inside of a bracketed construct an anchor at its far end.
insideBrackets :: Maybe Span -> Doc -> Doc
insideBrackets here d = d <> foldMap (emptyAnchor . endOf) here

-- | Lay a document out according to a span, and to the comments inside it.
grouped :: Ctx -> Span -> Doc -> Doc
grouped ctx s d
  | holdsLineComment ctx s = broken d
  | otherwise = group s d

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
