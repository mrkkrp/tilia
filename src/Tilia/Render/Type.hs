{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
    contextOf,
    tyVarBndr,
    Visibility (..),
    forallBndrs,
    forallTelescope,
    outerBndrs,
    recordFieldsAt,
    conDeclField,
    documentedConDeclField,
    strictness,
    typeArgument,
    typeArgSpan,
    typeIsDocumented,
    asSigType,
  )
where

import Data.Choice (Choice, fromBool, isTrue)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.Name.Occurrence (isTvOcc)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), getLoc, unLoc)
import GHC.Types.Var (Specificity (..))
import Tilia.Doc.Combinators
import Tilia.Fixity (Namespace (..))
import Tilia.Render.Context
import Tilia.Render.Haddock
import Tilia.Render.Literal (stringLiteral)
import Tilia.Render.Name
import Tilia.Render.Operator
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
    forallTelescope ctx tele <> betweenArgs <> hsType ctx t
  HsQualTy _ qs t ->
    context ctx qs
      <> space
      <> txt "=>"
      <> betweenArgs
      <> case unLoc t of
        HsQualTy{} -> recur (unLoc t)
        HsFunTy{} -> hsType ctx t
        _ -> at ctx t recur
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
    hsType ctx x
      <> space
      <> multiplicity (at_ ctx recur) multAnn
      <> space
      <> txt "->"
      <> betweenArgs
      <> case unLoc y of
        HsFunTy{} -> recur (unLoc y)
        _ -> at ctx y recur
  HsListTy _ t ->
    layoutWithin ctx here (spanOf t) $
      brackets (insideBrackets here (hsType ctx t))
  HsTupleTy _ sort xs ->
    layoutWithin ctx here (spansOf xs) $
      tupleBrackets sort (insideBrackets here (commaSep (fmap (align . hsType ctx) xs)))
  HsSumTy _ xs ->
    unboxed (sepBy (joinedBy "|") (fmap (align . hsType ctx) xs))
  HsOpTy _ _ x op y -> typeChain ctx x op y
  HsParTy _ t ->
    layoutWithin ctx here (spanOf t) (parens (insideBrackets here (hsType ctx t)))
  HsIParamTy _ n t ->
    align (at ctx n outputable <> joinedBy "::" <> indent (hsType ctx t))
  HsStarTy _ _ -> txt "*"
  HsKindSig _ t k ->
    align (hsType ctx t <> joinedBy "::" <> indent (hsType ctx k))
  HsSpliceTy _ splice -> knotSplice (ctxKnot ctx) ctx DollarSplice splice
  HsDocTy _ t str -> haddockInline ctx Pipe str <> hsType ctx t
  HsExplicitListTy _ promoted xs ->
    tick promoted
      <> brackets
        ( insideBrackets
            here
            (quoteGap promoted xs <> commaSep (fmap (align . hsType ctx) xs))
        )
  HsExplicitTupleTy _ promoted xs ->
    tick promoted
      <> parens
        ( insideBrackets
            here
            (quoteGap promoted xs <> commaSep (fmap (hsType ctx) xs))
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
    betweenArgs = if isTrue documented then hardBreak else breakOrSpace

-- | A chain of type operators, regrouped by precedence.
typeChain :: Ctx -> LHsType GhcPs -> LocatedN RdrName -> LHsType GhcPs -> Doc
typeChain ctx x op y =
  renderChain ctx (uncurry (associate fixity) (flattenAround split x op y))
  where
    split t = case unLoc t of
      HsOpTy _ _ l o r -> Just (l, o, r)
      _ -> Nothing
    fixity o = operatorFixity ctx InTypes (unLoc o)

-- | Print a chain of type operators once precedence has regrouped it.
renderChain :: Ctx -> OpChain (LHsType GhcPs) (LocatedN RdrName) -> Doc
renderChain ctx = \case
  Operand t -> hsType ctx t
  chain@(Chain (firstOne :| rest) operators) ->
    layoutFrom ctx (chainSpan spanOf chain) $
      renderChain ctx firstOne <> mconcat (zipWith piece operators rest)
  where
    piece op operand =
      attach Normal (name ctx op <> space <> renderChain ctx operand)

-- | Gather a nest of applications into a head and its arguments.
gatherAppArgs ::
  LHsType GhcPs ->
  [LHsType GhcPs] ->
  (LHsType GhcPs, [LHsType GhcPs])
gatherAppArgs f known = case unLoc f of
  HsAppTy _ l r -> gatherAppArgs l (r : known)
  _ -> (f, known)

-- | The brackets a tuple type is written with.
tupleBrackets :: HsTupleSort -> Doc -> Doc
tupleBrackets = \case
  HsUnboxedTuple -> unboxed
  HsBoxedOrConstraintTuple -> parens

-- | The tick in front of a promoted list or tuple.
tick :: PromotionFlag -> Doc
tick = \case
  IsPromoted -> txt "'"
  NotPromoted -> mempty

-- | The tick on a promoted name, held off it when the name itself begins
-- with one, since @''@ is the spelling of a type-level quote.
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
context ctx = at_ ctx (contextOf loneVariable (hsType ctx) . fmap unbracket)
  where
    loneVariable t = case unLoc t of
      HsTyVar _ _ (L _ n) -> isTvOcc (rdrNameOcc n)
      _ -> False
    unbracket t = case unLoc t of
      HsParTy _ inner -> unbracket inner
      _ -> t

-- | A context over anything that can stand as a constraint.
contextOf ::
  -- | Is this constraint nothing but a variable?
  (a -> Bool) ->
  -- | How to print one.
  (a -> Doc) ->
  -- | The constraints.
  [a] ->
  Doc
contextOf lone render = \case
  [] -> txt "()"
  [x] | lone x -> render x
  xs -> parens (commaSep (fmap (align . render) xs))

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
    binder = case tvb_var of
      HsBndrVar _ x -> name ctx x
      HsBndrWildCard _ -> txt "_"
    (kind, kinded) = case tvb_kind of
      HsBndrNoKind _ -> (mempty, False)
      HsBndrKind _ k ->
        (joinedBy "::" <> indent (hsType ctx k), True)
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
forallBndrs _ Invisible _ [] = txt "forall."
forallBndrs _ Visible _ [] = txt "forall ->"
forallBndrs ctx visibility render bndrs =
  layoutAcross ctx bndrs $
    txt "forall"
      <> breakOrSpace
      <> indent (align (sepBy breakOrSpace (fmap (align . at_ ctx render) bndrs)) <> close)
  where
    close = case visibility of
      Invisible -> txt "."
      Visible -> space <> txt "->"

-- | The @forall@ that opens a type.
forallTelescope :: Ctx -> HsForAllTelescope GhcPs -> Doc
forallTelescope ctx = \case
  HsForAllInvis _ bndrs -> forallBndrs ctx Invisible (tyVarBndr ctx) bndrs
  HsForAllVis _ bndrs -> forallBndrs ctx Visible (tyVarBndr ctx) bndrs

-- | The binders a signature quantifies over, when it names them.
outerBndrs :: Ctx -> HsOuterTyVarBndrs Specificity GhcPs -> Doc
outerBndrs ctx = \case
  HsOuterImplicit _ -> mempty
  HsOuterExplicit _ bndrs -> forallTelescope ctx (mkHsForAllInvisTele noAnn bndrs)

-- | A type together with whatever it quantifies over.
hsSigType :: Ctx -> LHsSigType GhcPs -> Doc
hsSigType ctx = at_ ctx (hsSigTypeBody ctx)

-- | A signature type whose location the caller has already entered.
hsSigTypeBody :: Ctx -> HsSigType GhcPs -> Doc
hsSigTypeBody ctx HsSig{..} =
  outerBndrs ctx sig_bndrs
    <> ( case sig_bndrs of
           HsOuterImplicit{} -> mempty
           HsOuterExplicit{} -> afterBinders
       )
    <> hsType ctx sig_body
  where
    afterBinders
      | typeIsDocumented (unLoc sig_body) = hardBreak
      | otherwise = breakOrSpace

-- | The @:: t@ that follows a name.
typeAscription :: Ctx -> LHsSigType GhcPs -> Doc
typeAscription ctx sigType =
  indent (space <> txt "::" <> separator <> hsSigType ctx sigType)
  where
    separator
      | typeIsDocumented (unLoc (sig_body (unLoc sigType))) = hardBreak
      | otherwise = breakOrSpace

-- | Give a plain type the shape of a signature type.
asSigType :: LHsType GhcPs -> LHsSigType GhcPs
asSigType ty = L (getLoc ty) (HsSig NoExtField (HsOuterImplicit NoExtField) ty)

-- | The braces of a record, and the fields inside them.
recordFieldsAt :: Ctx -> XRec GhcPs [LHsConDeclRecField GhcPs] -> Doc
recordFieldsAt ctx l = at ctx l (recordFields ctx (spanOf l))

-- | The fields of a record.
recordFields :: Ctx -> Maybe Span -> [LHsConDeclRecField GhcPs] -> Doc
recordFields ctx enclosing xs =
  brokenIfDocumented ctx xs . braces . insideBrackets enclosing $
    commaSep (fmap (align . at_ ctx (recordField ctx)) xs)

-- | One field of a record, documentation and multiplicity included.
recordField :: Ctx -> HsConDeclRecField GhcPs -> Doc
recordField ctx HsConDeclRecField{..} =
  foldMap (haddockInline ctx Pipe) (cdf_doc cdrf_spec)
    <> align (commaSep (fmap (at_ ctx (name ctx . foLabel)) cdrf_names))
    <> space
    <> multiplicity (hsType ctx) (cdf_multiplicity cdrf_spec)
    <> joinedBy "::"
    <> align (indent (conDeclField ctx cdrf_spec))

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
