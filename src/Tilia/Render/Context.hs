{-# LANGUAGE OverloadedStrings #-}

-- | What the syntax walk needs to know that the syntax tree does not say.
module Tilia.Render.Context
  ( -- * The context
    RenderConfig (..),
    defaultRenderConfig,
    Ctx (..),
    Knot (..),
    FamilyStyle (..),
    ArrowStyle (..),

    -- * Sites
    Site (..),
    plainSite,
    withBracing,
    leftmost,
    underSite,
    closingFor,

    -- * Extensions
    extensionOn,
    negativeLiteralGap,
    reserved,

    -- * Fixities
    operatorFixity,

    -- * What lies between two spans
    commentPrintedBetween,
    lineCommentWrittenBetween,
    underComment,
    remarkOnEachLine,
    commentRightUnder,
    remarkUnder,
    separatedByBlank,
    directiveBetween,

    -- * Entering the tree
    at,
    at_,
    atSpan,
    keywordAt,
    layoutFrom,
    layoutWithin,
    layoutAcross,
    writtenOnOneLine,
    attachOperator,
    joinedByArrow,
    lineLed,

    -- * Haddocks
    writtenHaddock,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs hiding (Fixity, UnicodeSyntax)
import GHC.LanguageExtensions.Type (Extension (NegativeLiterals, UnicodeSyntax))
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SrcLoc (GenLocated (..))
import GHC.Types.SrcLoc qualified as GHC

import Tilia.Comments
  ( Comment (..),
    CommentStyle (..),
    carriesOn,
    commentTrailing,
  )
import Tilia.Doc.Combinators
import Tilia.Fixity
  ( Fixity,
    Namespace (..),
    Resolution (..),
    Scope,
    lookupFixity,
    opName,
    qualifierOf,
  )
import Tilia.Render.Layout (Bracing (..))
import Tilia.Settings (ArrowStyle (..), Indentation, Settings, defaultSettings)
import Tilia.Source
  ( Source,
    SourceType,
    blankAt,
    directivePresentOnLine,
    lineAt,
    offsetOf,
    quoteEndingFrom,
    sourceLines,
  )
import Tilia.Span
import Tilia.Span.Ghc

----------------------------------------------------------------------------
-- The context

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
    -- | The names the module defines macros by, in whichever configuration.
    rcMacros :: Set Text,
    -- | Where the argument lists of the module's uses of its function-like
    -- macros were written, which cpp pastes into the macros as they are.
    rcMacroArguments :: Set Span,
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
      rcMacros = Set.empty,
      rcMacroArguments = Set.empty,
      rcSettings = defaultSettings
    }

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
    -- | The comments that end their line, by starting position.
    ctxLineComments :: Map (Int, Int) Comment,
    -- | The column of the trailing comment each comment that carries one on
    -- begins at, by where that comment begins.
    ctxCarryingOn :: Map (Int, Int) Int,
    -- | The module as its author wrote it.
    ctxSource :: Source,
    -- | The author's own text for each Haddock, by starting position.
    ctxHaddocks :: Map (Int, Int) Comment,
    -- | The knot.
    ctxKnot :: Knot,
    -- | How far each level of nesting is indented.
    ctxIndentation :: Indentation,
    -- | Where a list laid out on several lines puts its commas.
    ctxCommaStyle :: CommaStyle,
    -- | Where a type laid out on several lines puts its arrows.
    ctxArrowStyle :: ArrowStyle,
    -- | The names the module defines macros by, in whichever configuration.
    ctxMacros :: Set Text,
    -- | Where the argument lists of the module's uses of its function-like
    -- macros were written, which cpp pastes into the macros as they are.
    ctxMacroArguments :: Set Span
  }

-- | The backward edges of the rendering knot.
data Knot = Knot
  { -- | Expressions, needed by patterns.
    knotExpr :: Ctx -> LHsExpr GhcPs -> Doc,
    -- | Splices, needed by types and patterns, defined with expressions.
    knotSplice :: Ctx -> SpliceDecoration -> HsUntypedSplice GhcPs -> Doc,
    -- | Signature declarations, needed by @let@ and @where@ bodies.
    knotSig :: Ctx -> Sig GhcPs -> Doc,
    -- | A run of declarations, needed by Template Haskell brackets and by
    -- class and instance bodies.
    knotDecls :: Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc
  }

-- | Whether a declaration stands on its own or inside a class or an
-- instance.
data FamilyStyle
  = Associated
  | Free
  deriving (Eq, Show)

----------------------------------------------------------------------------
-- Sites

-- | What the surroundings of a node oblige it to do.
data Site = Site
  { -- | Is it followed by a line a step in, as @f@ may be by one with @a@
    -- in @f a@, and @x@ by one with @- 1@ in @x - 1@?
    --
    -- A layout block it ends with puts its items a further step in, so that
    -- the layout rule ends the block before that line.
    siteFollowed :: Bool,
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
    { siteFollowed = False,
      siteInBlock = False,
      siteBracing = NoBrace
    }

-- | The same site, with a different answer about braces.
withBracing :: Bracing -> Site -> Site
withBracing bracing site = site{siteBracing = bracing}

-- | The site of the part a node begins with, which begins whatever item of
-- a layout block the node begins.
leftmost ::
  -- | The site of the node.
  Site ->
  Site
leftmost site = plainSite{siteInBlock = siteInBlock site}

-- | Indent a hanging body, one step further where a line a step in follows.
underSite ::
  -- | The site of the node the body hangs off.
  Site ->
  -- | The body.
  Doc ->
  Doc
underSite site =
  nest (if siteFollowed site then 2 else 1)

-- | Where a bracket opened here has to close.
closingFor :: Site -> ClosingIndent
closingFor site = if siteInBlock site then Indented else Outdented

----------------------------------------------------------------------------
-- Extensions

-- | Is the extension on?
extensionOn :: Ctx -> Extension -> Bool
extensionOn ctx e = Set.member e (ctxExtensions ctx)

-- | With @NegativeLiterals@ on, @- 1@ and @-1@ are different, so the minus
-- in front of a literal has to keep its distance.
negativeLiteralGap :: Ctx -> Doc
negativeLiteralGap ctx = includeWhen (extensionOn ctx NegativeLiterals) space

-- | A reserved symbol in its @UnicodeSyntax@ form, where that extension is
-- in force and the symbol has one.
reserved :: Ctx -> Text -> Text
reserved ctx symbol
  | extensionOn ctx UnicodeSyntax = case symbol of
      "::" -> "∷"
      "=>" -> "⇒"
      "->" -> "→"
      "<-" -> "←"
      "forall" -> "∀"
      "-<" -> "⤙"
      ">-" -> "⤚"
      "-<<" -> "⤛"
      ">>-" -> "⤜"
      "(|" -> "⦇"
      "|)" -> "⦈"
      "*" -> "★"
      _ -> symbol
  | otherwise = symbol
{-# INLINE reserved #-}

----------------------------------------------------------------------------
-- Fixities

-- | The fixity of an operator, if one was established.
operatorFixity :: Ctx -> Namespace -> RdrName -> Maybe Fixity
operatorFixity ctx namespace name = do
  scope <- ctxScope ctx
  case lookupFixity scope namespace (qualifierOf name) (opName name) of
    Resolved fixity _ -> Just fixity
    Unresolved _ -> Nothing

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
        || maybe
          False
          (\remark -> carriesOn remark (commentSpan c) (Just b))
          (Map.lookup (startPoint (commentSpan c)) (ctxCarryingOn ctx))

-- | Is a comment going to be printed between the two spans?
commentPrintedBetween :: Ctx -> Maybe Span -> Maybe Span -> Bool
commentPrintedBetween ctx a b = nextPrinted ctx a b /= b

-- | Was a comment that ends its line written between the two spans, wherever
-- it is printed?
lineCommentWrittenBetween :: Ctx -> Maybe Span -> Maybe Span -> Bool
lineCommentWrittenBetween ctx (Just a) (Just b) =
  holdsLineComment ctx (mkSpan (endPoint a) (startPoint b))
lineCommentWrittenBetween _ _ _ = False

-- | What follows a token on its line, a step in where a comment written
-- between the two ends the line, so that it does not begin the next line at
-- the indentation in force, where a layout block would take it for an item.
underComment ::
  -- | The context.
  Ctx ->
  -- | The token.
  Maybe Span ->
  -- | What follows it.
  Maybe Span ->
  -- | What follows it, printed.
  Doc ->
  Doc
underComment ctx token part
  | lineCommentWrittenBetween ctx token part = indent
  | otherwise = id

-- | Was a comment that ends its line written between the two spans, and
-- another on the line the second begins, so that joining the two lines
-- would take one of them off its line?
remarkOnEachLine ::
  -- | The context.
  Ctx ->
  -- | What comes first.
  Maybe Span ->
  -- | What comes next.
  Maybe Span ->
  Bool
remarkOnEachLine ctx a b =
  lineCommentWrittenBetween ctx a b
    && lineCommentWrittenBetween ctx (startOf <$> b) (nextLine <$> b)
  where
    nextLine s = mkSpan (spanStartLine s + 1, 1) (spanStartLine s + 1, 1)

-- | Do the comments printed between the two spans begin right under the
-- first, with an empty line the author left under one of them and no
-- preprocessor directive anywhere between?
remarkUnder ::
  -- | The context.
  Ctx ->
  -- | What comes first.
  Maybe Span ->
  -- | What comes next.
  Maybe Span ->
  Bool
remarkUnder ctx ma@(Just a) mb@(Just b) =
  not (separatedByBlank ctx ma mb)
    && not (directiveBetween ctx ma mb)
    && any
      (writtenBlank ctx . (+ 1) . spanEndLine . commentSpan)
      (printedBetween ctx a b)
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

-- | Is a preprocessor directive written between the two spans?
directiveBetween ::
  -- | The context.
  Ctx ->
  -- | What comes first.
  Maybe Span ->
  -- | What comes next.
  Maybe Span ->
  Bool
directiveBetween ctx (Just a) (Just b) =
  any
    (`directivePresentOnLine` sourceLines (ctxSource ctx))
    [spanEndLine a + 1 .. spanStartLine b - 1]
directiveBetween _ _ _ = False

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
-- it settles the node's layout from the region it occupied. A node written
-- as the argument list of a use of a function-like macro is printed as it
-- was written, since cpp pastes that into the macro's body.
at :: HasLoc l => Ctx -> GenLocated l a -> (a -> Doc) -> Doc
at ctx l f = atSpan ctx s $ case s of
  Just x
    | not (Set.null pasted),
      Set.member x pasted,
      Just written <- writtenAt ctx x ->
        txt written
  _ -> f (GHC.unLoc l)
  where
    s = spanOf l
    pasted = ctxMacroArguments ctx

-- | 'at' with the arguments the other way round, for use in sections.
at_ :: HasLoc l => Ctx -> (a -> Doc) -> GenLocated l a -> Doc
at_ ctx f l = at ctx l f

-- | Lay a region out as 'layoutFrom' does, and claim it.
--
-- Claiming is the difference between this and 'layoutFrom': a comment
-- written anywhere inside the region attaches to this document. So it is
-- for the handful of things a comment can be written against that the
-- syntax tree gives no node for—the @where@ that opens a body, the @then@
-- of an @if@—and for nothing else.
atSpan :: Ctx -> Maybe Span -> Doc -> Doc
atSpan _ Nothing d = d
atSpan ctx (Just s) d = located s (layoutFrom ctx (Just s) d)

-- | A keyword, claiming the span it was written on.
keywordAt :: Ctx -> Maybe Span -> Text -> Doc
keywordAt ctx s = atSpan ctx s . txt

-- | Lay a region out broken where it was written over several lines, and
-- otherwise flat where it fits within the column limit, claiming nothing.
--
-- The lines of a quasi-quotation count as one, since they are reproduced
-- rather than laid out.
layoutFrom :: Ctx -> Maybe Span -> Doc -> Doc
layoutFrom ctx s
  | maybe True (writtenOnOneLine ctx) s = frangible
  | otherwise = broken

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
  | otherwise = layoutFrom ctx contents d

-- | 'layoutFrom' over the region several located things cover.
layoutAcross :: HasLoc l => Ctx -> [GenLocated l a] -> Doc -> Doc
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

-- | The @::@, @=>@ or @->@ that joins two parts of a type, at the end of a
-- line or at the start of the next one, as the project asks.
--
-- A leading arrow is one fragment with the space after it, which the
-- preprocessor merge tells from a trailing one by that space.
joinedByArrow ::
  -- | The context.
  Ctx ->
  -- | Where the line may break.
  Doc ->
  -- | What the arrow follows.
  Maybe Span ->
  -- | What it leads to.
  Maybe Span ->
  -- | The arrow.
  Text ->
  Doc
joinedByArrow ctx separator before after arrow = case ctxArrowStyle ctx of
  TrailingArrows -> space <> txt arrow <> separator
  LeadingArrows -> separator <> lineLed before after <> txt (arrow <> " ")

-- | A region over the indentation of the line a part begins, where that
-- line is below the part before it, so that the comments written above it
-- come before what leads it.
lineLed ::
  -- | The part before.
  Maybe Span ->
  -- | The part.
  Maybe Span ->
  Doc
lineLed before after = case (before, after) of
  (Just b, Just a) | spanEndLine b < spanStartLine a -> indentation a
  _ -> mempty

-- | A region over the indentation of the line a span begins on.
indentation :: Span -> Doc
indentation s = located (mkSpan (spanStartLine s, 1) (startPoint s)) mempty

-- | Was the region written on one line, the lines of a quasi-quotation
-- counting as one?
writtenOnOneLine :: Ctx -> Span -> Bool
writtenOnOneLine ctx s = from (spanStartLine s)
  where
    from n =
      n >= spanEndLine s
        || maybe False from (quoteEndingFrom n (ctxSource ctx))

-- | What was written at a span within one line.
writtenAt :: Ctx -> Span -> Maybe Text
writtenAt ctx s = do
  l <- lineAt (spanStartLine s) (sourceLines (ctxSource ctx))
  let from = offsetOf l (spanStartColumn s)
  pure (T.take (offsetOf l (spanEndColumn s) - from) (T.drop from l))

-- | Does a comment that ends its line begin inside this span?
holdsLineComment :: Ctx -> Span -> Bool
holdsLineComment ctx s =
  case Map.lookupGE (startPoint s) (ctxLineComments ctx) of
    Just (start, _) -> start < endPoint s
    Nothing -> False

----------------------------------------------------------------------------
-- Haddocks

-- | The Haddock the author wrote at this position, if we kept it.
writtenHaddock :: Ctx -> Maybe Span -> Maybe Comment
writtenHaddock ctx ms = do
  s <- ms
  c <- Map.lookup (startPoint s) (ctxHaddocks ctx)
  case commentStyle c of
    DocComment -> Just c
    _ -> Nothing
