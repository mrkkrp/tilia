{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types.
module Tilia.Render.Type
  ( hsType,
    hsTypeBody,
    hsSigType,
    hsSigTypeBody,
    typeAscription,
    context,
    leftContext,
    contextOf,
    tyVarBndr,
    Visibility (..),
    forallBndrs,
    quantifier,
    forallTelescope,
    outerBndrs,
    recordFieldsAt,
    conDeclField,
    documentedConDeclField,
    strictness,
    typeArgument,
    typeArgSpan,
    typeIsDocumented,
    signaturePart,
    documentedParts,
    typeBreak,
    asSigType,
  )
where

import Data.Choice (Choice, fromBool, isTrue)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Hs hiding (UnicodeSyntax)
import GHC.LanguageExtensions.Type (Extension (Cpp, UnicodeSyntax))
import GHC.Types.Name.Occurrence (isTvOcc, occNameString)
import GHC.Types.Name.Reader (RdrName, isUnqual, rdrNameOcc)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), getLoc, unLoc)
import GHC.Types.Var (Specificity (..))

import Tilia.Doc.Combinators
import Tilia.Fixity (typeOperatorNamespace)
import Tilia.Render.Context
import Tilia.Render.Haddock
import Tilia.Render.Literal (stringLiteral)
import Tilia.Render.Name
import Tilia.Render.Operator
import Tilia.Source (definedMacros, sourceLines)
import Tilia.Span
import Tilia.Span.Ghc

-- | A type.
hsType :: Ctx -> LHsType GhcPs -> Doc
hsType ctx l = at ctx l (hsTypeBody ctx (spanOf l))

-- | A type whose location the caller has already entered.
hsTypeBody :: Ctx -> Maybe Span -> HsType GhcPs -> Doc
hsTypeBody ctx here t = typeBody ctx (fromBool (typeIsDocumented t)) here t

-- | The body of a type, with the decision about its arguments handed down.
typeBody ::
  -- | The context.
  Ctx ->
  -- | Does the whole type carry documentation?
  Choice "documented" ->
  -- | Where the type was written.
  Maybe Span ->
  -- | The type.
  HsType GhcPs ->
  Doc
typeBody ctx documented here = \case
  HsForAllTy _ tele t ->
    (if isTrue documented then broken else id) $
      forallTelescope ctx betweenArgs (spanOf t) tele
        <> signaturePart ctx (hsType ctx) t
  HsQualTy _ qs t ->
    context ctx qs
      <> joinedByArrow
        ctx
        betweenArgs
        (spanOf qs)
        (spanOf t)
        (reserved ctx "=>")
      <> case unLoc t of
        HsQualTy{} -> recur (unLoc t)
        HsFunTy{} -> hsType ctx t
        _ -> signaturePart ctx (at_ ctx recur) t
  HsTyVar _ promoted n -> promotion promoted n <> name ctx n
  HsAppTy _ f x ->
    let (func, args) = gatherAppArgs f [x]
     in layoutFrom ctx (spanOf f <> spansOf args) . align $
          hsType ctx func
            <> breakOrSpace
            <> indent (sepBy breakOrSpace (fmap (hsType ctx) args))
  HsAppKindTy _ ty kd ->
    align (hsType ctx ty <> breakOrSpace <> indent (txt "@" <> hsType ctx kd))
  HsFunTy _ multAnn x y ->
    signaturePart ctx (hsType ctx) x
      <> joinedByFunctionArrow
        ctx
        (at_ ctx recur)
        betweenArgs
        (spanOf x)
        (spanOf y)
        multAnn
      <> fenced result
    where
      result = case unLoc y of
        HsFunTy{} -> recur (unLoc y)
        _ -> signaturePart ctx (at_ ctx recur) y
      fenced = case ctxArrowStyle ctx of
        TrailingArrows -> fence (fmap endOf (arrowSpan multAnn) <> spanOf y)
        LeadingArrows -> id
  HsListTy _ t ->
    layoutWithin ctx here (spanOf t) $
      brackets (insideBrackets here (hsType ctx t))
  HsTupleTy _ sort xs ->
    layoutWithin ctx here (spansOf xs) $
      tupleBrackets
        (ctxCommaStyle ctx)
        sort
        ( insideBrackets
            here
            (commaSep (ctxCommaStyle ctx) (fmap (align . hsType ctx) xs))
        )
  HsSumTy _ xs ->
    unboxed (sepBy (joinedBy "|") (fmap (align . hsType ctx) xs))
  HsOpTy _ _ x op y -> typeChain ctx x op y
  HsParTy _ t ->
    layoutWithin
      ctx
      here
      (spanOf t)
      (parens (insideBrackets here (hsType ctx t)))
  HsIParamTy _ n t ->
    align (at ctx n outputable <> typeAscription ctx (spanOf n) (asSigType t))
  HsStarTy _ _ -> txt (reserved ctx "*")
  HsKindSig _ t k ->
    align (hsType ctx t <> typeAscription ctx (spanOf t) (asSigType k))
  HsSpliceTy _ splice -> knotSplice (ctxKnot ctx) ctx DollarSplice splice
  HsDocTy _ t str -> haddockInline ctx Pipe str <> hsType ctx t
  HsExplicitListTy _ promoted xs ->
    tick promoted
      <> commaBracketWith
        (ctxCommaStyle ctx)
        Outdented
        "["
        "]"
        ( insideBrackets
            here
            ( quoteGap promoted xs
                <> commaSep (ctxCommaStyle ctx) (fmap (align . hsType ctx) xs)
            )
        )
  HsExplicitTupleTy _ promoted xs ->
    tick promoted
      <> commaBracketWith
        (ctxCommaStyle ctx)
        Outdented
        "("
        ")"
        ( insideBrackets
            here
            ( quoteGap promoted xs
                <> commaSep (ctxCommaStyle ctx) (fmap (align . hsType ctx) xs)
            )
        )
  HsTyLit _ t -> case t of
    HsStrTy (SourceText s) _ -> stringLiteral s
    other -> outputable other
  HsWildCardTy _ -> txt "_"
  XHsType ext -> case ext of
    HsCoreTy t -> outputable t
    HsBangTy _ (HsSrcBang _ unpacked strict) t ->
      unpackPragma unpacked <> strictness strict <> hsType ctx t
    HsRecTy _ fields -> recordFields ctx Nothing fields
  where
    recur = typeBody ctx documented Nothing
    betweenArgs = typeBreak (isTrue documented)

-- | A part of a signature's type that goes on a line of its own, with its
-- Haddock under it where arrows lead, since above it the Haddock would come
-- between the part and the arrow in front of it.
signaturePart ::
  -- | The context.
  Ctx ->
  -- | How to print a part with no Haddock.
  (LHsType GhcPs -> Doc) ->
  -- | The part.
  LHsType GhcPs ->
  Doc
signaturePart ctx render l = case unLoc l of
  HsDocTy _ t str
    | ctxArrowStyle ctx == LeadingArrows ->
        hsType ctx t <> hardBreak <> haddock ctx Caret Open str
  _ -> render l

-- | The parts of a signature's type that carry a Haddock and end a line of
-- the type as written, so that what is written under them comes out under
-- the Haddock.
documentedParts :: LHsType GhcPs -> [LHsType GhcPs]
documentedParts = go Nothing
  where
    go next l = case unLoc l of
      HsDocTy{} -> [l | not (sameLine (spanOf l) next)]
      HsForAllTy _ _ t -> go Nothing t
      HsQualTy _ _ t -> go Nothing t
      HsFunTy _ _ x y -> go (spanOf y) x <> go Nothing y
      _ -> []

-- | A chain of type operators, regrouped by precedence.
typeChain :: Ctx -> LHsType GhcPs -> LocatedN RdrName -> LHsType GhcPs -> Doc
typeChain ctx x op y =
  renderChain ctx (uncurry (associate fixity) (flattenAround split x op y))
  where
    split t = case unLoc t of
      HsOpTy _ _ l o r -> Just (l, o, r)
      _ -> Nothing
    fixity o = operatorFixity ctx (typeOperatorNamespace (unLoc o)) (unLoc o)

-- | Print a chain of type operators once precedence has regrouped it.
renderChain :: Ctx -> OpChain (LHsType GhcPs) (LocatedN RdrName) -> Doc
renderChain ctx = \case
  Operand t -> hsType ctx t
  chain@(Chain operands@(firstOne :| _) operators) ->
    layoutFrom ctx (chainSpan spanOf chain) $
      renderChain ctx firstOne <> foldMap piece (links operands operators)
  where
    piece (previous, op, operand, _) =
      attachOperator
        Normal
        (chainSpan spanOf previous)
        (spanOf op)
        (chainSpan spanOf operand)
        (name ctx op)
        (renderChain ctx operand)

-- | Gather a nest of applications into a head and its arguments.
gatherAppArgs ::
  LHsType GhcPs ->
  [LHsType GhcPs] ->
  (LHsType GhcPs, [LHsType GhcPs])
gatherAppArgs f known = case unLoc f of
  HsAppTy _ l r -> gatherAppArgs l (r : known)
  _ -> (f, known)

-- | The brackets a tuple type is written with.
tupleBrackets ::
  -- | Where the commas go.
  CommaStyle ->
  -- | Which kind of tuple.
  HsTupleSort ->
  -- | The items, separated.
  Doc ->
  Doc
tupleBrackets commas = \case
  HsUnboxedTuple -> unboxed
  HsBoxedOrConstraintTuple -> commaBracketWith commas Outdented "(" ")"

-- | The tick in front of a promoted list or tuple.
tick :: PromotionFlag -> Doc
tick = \case
  IsPromoted -> txt "'"
  NotPromoted -> mempty

-- | The tick on a promoted name, held off it where the name's second
-- character is a tick too, as @'B'@ would read as a character literal.
promotion :: PromotionFlag -> LocatedN RdrName -> Doc
promotion NotPromoted _ = mempty
promotion IsPromoted n =
  txt "'" <> includeWhen (beginsWithTick (showGhc (unLoc n))) space
  where
    beginsWithTick shown = case T.uncons (T.drop 1 shown) of
      Just ('\'', _) -> True
      _ -> False

-- | A promoted list or tuple whose first element is itself promoted needs a
-- space, or @'['a]@ would begin with a character literal.
quoteGap :: PromotionFlag -> [LHsType GhcPs] -> Doc
quoteGap IsPromoted (t : _) | startsWithTick (unLoc t) = space
quoteGap _ _ = mempty

-- | Does this type begin with a tick of its own?
startsWithTick :: HsType GhcPs -> Bool
startsWithTick = \case
  HsAppTy _ (L _ f) _ -> startsWithTick f
  HsTyVar _ IsPromoted _ -> True
  HsExplicitTupleTy{} -> True
  HsExplicitListTy{} -> True
  HsTyLit _ HsCharTy{} -> True
  _ -> False

-- | The pragma asking for a field to be unpacked, or not to be.
unpackPragma :: SrcUnpackedness -> Doc
unpackPragma = \case
  SrcUnpack -> txt "{-# UNPACK #-}" <> space
  SrcNoUnpack -> txt "{-# NOUNPACK #-}" <> space
  NoSrcUnpack -> mempty

-- | The @!@ or @~@ in front of a field.
strictness :: SrcStrictness -> Doc
strictness = \case
  SrcLazy -> txt "~"
  SrcStrict -> txt "!"
  NoSrcStrict -> mempty

-- | The break between the parts of a type, a hard one where the type
-- carries documentation, which ends its line.
typeBreak :: Bool -> Doc
typeBreak documented = if documented then hardBreak else breakOrSpace

-- | Does any argument of this type carry documentation?
typeIsDocumented :: HsType GhcPs -> Bool
typeIsDocumented = any documented . spine
  where
    documented = \case
      HsDocTy{} -> True
      _ -> False

-- | The pieces of a type that a signature would put on lines of their own:
-- the argument and result types, and whatever a @forall@ or a context is
-- wrapped around.
--
-- Not the types nested inside those. A Haddock written on an element of a
-- list argument documents the element and says nothing about how the
-- signature it sits in should be laid out.
spine :: HsType GhcPs -> [HsType GhcPs]
spine t =
  t : case t of
    HsFunTy _ _ a b -> spine (unLoc a) <> spine (unLoc b)
    HsForAllTy _ _ b -> spine (unLoc b)
    HsQualTy _ _ b -> spine (unLoc b)
    _ -> []

-- | A class context, as it appears before a @=>@.
context :: Ctx -> LHsContext GhcPs -> Doc
context ctx l =
  at ctx l $
    contextOf (ctxCommaStyle ctx) (spanOf l) bare (hsType ctx)
      . fmap unbracket
  where
    bare t = case unLoc t of
      HsTyVar _ _ (L _ n) | isTvOcc (rdrNameOcc n) -> True
      _ -> headedByMacro t
    headedByMacro t = case unLoc t of
      HsTyVar _ _ (L _ n) -> macro n
      HsAppTy _ f _ -> headedByMacro f
      _ -> False
    macro n =
      extensionOn ctx Cpp
        && isUnqual n
        && Set.member
          (T.pack (occNameString (rdrNameOcc n)))
          (definedMacros (sourceLines (ctxSource ctx)))
    unbracket t = case unLoc t of
      HsParTy _ inner -> unbracket inner
      _ -> t

-- | A context standing to the left of a @=>@, with the arrow and the break
-- beside it.
leftContext ::
  -- | The context.
  Ctx ->
  -- | What the arrow leads to.
  Maybe Span ->
  -- | The constraints.
  LHsContext GhcPs ->
  Doc
leftContext ctx after ctxt =
  context ctx ctxt
    <> joinedByArrow ctx breakOrSpace (spanOf ctxt) after (reserved ctx "=>")

-- | A context over anything that can stand as a constraint.
contextOf ::
  -- | Where the commas go.
  CommaStyle ->
  -- | Where the context was written.
  Maybe Span ->
  -- | Can this constraint, when it is the only one, go without parentheses?
  (a -> Bool) ->
  -- | How to print one.
  (a -> Doc) ->
  -- | The constraints.
  [a] ->
  Doc
contextOf commas here bare render = \case
  [] -> txt "()"
  [x] | bare x -> render x
  xs ->
    commaBracketWith commas Outdented "(" ")" . insideBrackets here $
      commaSep commas (fmap (align . render) xs)

-- | The flags a type variable binder may carry.
--
-- Three kinds of binder exist with three different flag types, and each
-- decides both whether the binder is inferred—which is what braces around it
-- mean—and whether anything is printed in front of it.
class TyVarBndrFlag flag where
  flagIsInferred :: flag -> Bool
  flagPrefix :: flag -> Doc
  flagPrefix _ = mempty

instance TyVarBndrFlag () where
  flagIsInferred () = False

instance TyVarBndrFlag Specificity where
  flagIsInferred = \case
    InferredSpec -> True
    SpecifiedSpec -> False

instance TyVarBndrFlag (HsBndrVis GhcPs) where
  flagIsInferred _ = False
  flagPrefix = \case
    HsBndrRequired NoExtField -> mempty
    HsBndrInvisible _ -> txt "@"

-- | One type variable binder.
tyVarBndr :: (TyVarBndrFlag flag) => Ctx -> HsTyVarBndr flag GhcPs -> Doc
tyVarBndr ctx HsTvb{..} = flagPrefix tvb_flag <> enclosed (binder <> kind)
  where
    (binder, binderSpan) = case tvb_var of
      HsBndrVar _ x -> (name ctx x, spanOf x)
      HsBndrWildCard _ -> (txt "_", Nothing)
    (kind, kinded) = case tvb_kind of
      HsBndrNoKind _ -> (mempty, False)
      HsBndrKind _ k -> (typeAscription ctx binderSpan (asSigType k), True)
    enclosed
      | flagIsInferred tvb_flag = braces
      | kinded = parens
      | otherwise = id

-- | Whether a @forall@ binds visibly.
data Visibility
  = -- | @forall a.@
    Invisible
  | -- | @forall a ->@
    Visible
  deriving (Eq, Show)

-- | The variables of a @forall@, with the punctuation that closes it.
forallBndrs ::
  (HasLoc l) =>
  -- | The context.
  Ctx ->
  -- | Does it bind visibly?
  Visibility ->
  -- | How to print one.
  (a -> Doc) ->
  -- | The variables it binds.
  [GenLocated l a] ->
  Doc
forallBndrs ctx visibility render bndrs =
  forallOver ctx render bndrs (forallClose ctx visibility bndrs)

-- | @forall@ and its variables, with what follows the last of them.
forallOver ::
  (HasLoc l) =>
  -- | The context.
  Ctx ->
  -- | How to print one.
  (a -> Doc) ->
  -- | The variables it binds.
  [GenLocated l a] ->
  -- | What follows them.
  Doc ->
  Doc
forallOver ctx _ [] close = txt (reserved ctx "forall") <> close
forallOver ctx render bndrs close =
  layoutAcross ctx bndrs $
    txt (reserved ctx "forall")
      <> breakOrSpace
      <> indent
        ( align (sepBy breakOrSpace (fmap (align . at_ ctx render) bndrs))
            <> close
        )

-- | The punctuation that closes a @forall@ on the line of its variables.
forallClose ::
  -- | The context.
  Ctx ->
  -- | Does it bind visibly?
  Visibility ->
  -- | The variables it binds.
  [a] ->
  Doc
forallClose ctx visibility bndrs = case visibility of
  Invisible ->
    includeWhen (null bndrs && extensionOn ctx UnicodeSyntax) space
      <> txt "."
  Visible -> space <> txt (reserved ctx "->")

-- | A @forall@ in a type, with the break between it and what it
-- quantifies.
quantifier ::
  (TyVarBndrFlag flag) =>
  -- | The context.
  Ctx ->
  -- | Where the line may break.
  Doc ->
  -- | Does it bind visibly?
  Visibility ->
  -- | What it quantifies.
  Maybe Span ->
  -- | The variables it binds.
  [LHsTyVarBndr flag GhcPs] ->
  Doc
quantifier ctx separator visibility after bndrs =
  case (ctxArrowStyle ctx, visibility) of
    (TrailingArrows, _) ->
      forallBndrs ctx visibility (tyVarBndr ctx) bndrs <> separator
    (LeadingArrows, Invisible) ->
      unclosed
        <> variant
          (forallClose ctx Invisible bndrs <> space)
          (separator <> lineLed (spansOf bndrs) after <> txt " . ")
    (LeadingArrows, Visible) ->
      unclosed
        <> joinedByArrow
          ctx
          separator
          (spansOf bndrs)
          after
          (reserved ctx "->")
  where
    unclosed = forallOver ctx (tyVarBndr ctx) bndrs mempty

-- | Where the arrow of a function type is written.
arrowSpan :: HsMultAnnOf mult GhcPs -> Maybe Span
arrowSpan = \case
  HsUnannotated a -> arrow a
  HsLinearAnn (EpPct1 _ a) -> arrow a
  HsLinearAnn (EpLolly t) -> spanOf t
  HsExplicitMult (_, a) _ -> arrow a
  where
    arrow = \case
      EpArrow t -> spanOf t
      _ -> Nothing

-- | The @forall@ that opens a type, with the break between it and what it
-- quantifies.
forallTelescope ::
  -- | The context.
  Ctx ->
  -- | Where the line may break.
  Doc ->
  -- | What it quantifies.
  Maybe Span ->
  -- | The @forall@.
  HsForAllTelescope GhcPs ->
  Doc
forallTelescope ctx separator after = \case
  HsForAllInvis _ bndrs -> quantifier ctx separator Invisible after bndrs
  HsForAllVis _ bndrs -> quantifier ctx separator Visible after bndrs

-- | The binders a signature or an equation quantifies over, where it names
-- them, with the break between them and what they scope over.
outerBndrs ::
  (TyVarBndrFlag flag) =>
  -- | The context.
  Ctx ->
  -- | Where the line may break.
  Doc ->
  -- | What they scope over.
  Maybe Span ->
  -- | The binders.
  HsOuterTyVarBndrs flag GhcPs ->
  Doc
outerBndrs ctx separator after = \case
  HsOuterImplicit _ -> mempty
  HsOuterExplicit _ bndrs -> quantifier ctx separator Invisible after bndrs

-- | A type together with whatever it quantifies over.
hsSigType :: Ctx -> LHsSigType GhcPs -> Doc
hsSigType ctx l = at ctx l (hsSigTypeBody ctx)

-- | A signature type whose location the caller has already entered.
hsSigTypeBody :: Ctx -> HsSigType GhcPs -> Doc
hsSigTypeBody ctx = sigTypeWith ctx (hsType ctx)

-- | A signature type with its body printed by the given function.
sigTypeWith ::
  -- | The context.
  Ctx ->
  -- | How to print the body.
  (LHsType GhcPs -> Doc) ->
  -- | The signature type.
  HsSigType GhcPs ->
  Doc
sigTypeWith ctx body HsSig{..} =
  (if documented then broken else id) $
    outerBndrs ctx (typeBreak documented) (spanOf sig_body) sig_bndrs
      <> body sig_body
  where
    documented = typeIsDocumented (unLoc sig_body)

-- | The @:: t@ that follows a name.
typeAscription ::
  -- | The context.
  Ctx ->
  -- | What the type is given to.
  Maybe Span ->
  -- | The type.
  LHsSigType GhcPs ->
  Doc
typeAscription ctx before sigType =
  indent
    ( joinedByArrow ctx separator before (spanOf sigType) (reserved ctx "::")
        <> at ctx sigType (sigTypeWith ctx (signaturePart ctx (hsType ctx)))
    )
  where
    separator = typeBreak (typeIsDocumented (unLoc (sig_body (unLoc sigType))))

-- | Give a plain type the shape of a signature type.
asSigType :: LHsType GhcPs -> LHsSigType GhcPs
asSigType ty = L (getLoc ty) (HsSig NoExtField (HsOuterImplicit NoExtField) ty)

-- | The braces of a record, and the fields inside them.
recordFieldsAt :: Ctx -> XRec GhcPs [LHsConDeclRecField GhcPs] -> Doc
recordFieldsAt ctx l = at ctx l (recordFields ctx (spanOf l))

-- | The fields of a record, each documented above it or, with leading
-- commas, below it, under the comma in front of it.
recordFields :: Ctx -> Maybe Span -> [LHsConDeclRecField GhcPs] -> Doc
recordFields ctx enclosing xs =
  brokenIfDocumented ctx xs
    . commaBracketWith commas Outdented "{" "}"
    . insideBrackets enclosing
    $ commaSep commas (fmap field xs)
  where
    commas = ctxCommaStyle ctx
    field = case commas of
      TrailingCommas ->
        align
          . at_
            ctx
            ( \f ->
                foldMap (haddockInline ctx Pipe) (documentation f)
                  <> recordField ctx f
            )
      LeadingCommas -> \l ->
        align (at_ ctx (recordField ctx) l)
          <> foldMap
            (\d -> hardBreak <> haddock ctx Caret Open d)
            (documentation (unLoc l))
    documentation :: HsConDeclRecField GhcPs -> Maybe (LHsDoc GhcPs)
    documentation HsConDeclRecField{cdrf_spec} = cdf_doc cdrf_spec

-- | One field of a record, multiplicity included, without its
-- documentation.
recordField :: Ctx -> HsConDeclRecField GhcPs -> Doc
recordField ctx HsConDeclRecField{..} =
  align
    ( commaSep
        (ctxCommaStyle ctx)
        (fmap (at_ ctx (name ctx . foLabel)) cdrf_names)
    )
    <> space
    <> multiplicity (hsType ctx) (cdf_multiplicity cdrf_spec)
    <> indent
      ( joinedByArrow
          ctx
          breakOrSpace
          (spansOf cdrf_names)
          (spanOf (cdf_type cdrf_spec))
          (reserved ctx "::")
          <> conDeclField ctx cdrf_spec
      )

-- | A constructor field, without its documentation or its multiplicity.
conDeclField :: Ctx -> HsConDeclField GhcPs -> Doc
conDeclField ctx CDF{..} =
  unpackPragma cdf_unpack
    <> at
      ctx
      cdf_type
      (\ty -> strictness cdf_bang <> hsTypeBody ctx (spanOf cdf_type) ty)

-- | A constructor field with its documentation in front of it.
documentedConDeclField :: Ctx -> HsConDeclField GhcPs -> Doc
documentedConDeclField ctx cdf =
  foldMap (haddockInline ctx Pipe) (cdf_doc cdf) <> conDeclField ctx cdf

-- | One argument on the left of a family or data instance.
typeArgument :: Ctx -> LHsTypeArg GhcPs -> Doc
typeArgument ctx = \case
  HsValArg NoExtField ty -> hsType ctx ty
  HsTypeArg _ ty -> txt "@" <> hsType ctx ty
  HsArgPar _ -> error "Tilia: HsArgPar is not expected in parsed source"

-- | Where an argument was.
typeArgSpan :: LHsTypeArg GhcPs -> Maybe Span
typeArgSpan = spanOfSrcSpan . lhsTypeArgSrcSpan
