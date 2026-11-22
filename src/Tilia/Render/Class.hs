{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Classes, instances and families.
module Tilia.Render.Class
  ( classDecl,
    clsInstDecl,
    tyFamInstDecl,
    dataFamInstDecl,
    standaloneDerivDecl,
    famDecl,
    roleAnnot,
  )
where

import Data.Choice (fromBool)
import Data.Foldable (fold)
import Data.Maybe (isNothing)
import GHC.Builtin.Types (cTupleTyConName, isCTupleTyConName)
import GHC.Core.Coercion.Axiom (Role (..))
import GHC.Hs
import GHC.Types.Fixity (LexicalFixity (..))
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SrcLoc (GenLocated (..), unLoc)

import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Data (dataDecl)
import Tilia.Render.Name
import Tilia.Render.Pragma
import Tilia.Render.Type
import Tilia.Span (Span, endOf)
import Tilia.Span.Ghc

-- | A type class declaration.
classDecl ::
  -- | The context.
  Ctx ->
  -- | Where its keywords were.
  AnnClassDecl ->
  -- | Its superclasses.
  Maybe (LHsContext GhcPs) ->
  -- | Its name.
  LocatedN RdrName ->
  -- | Its type variables.
  LHsQTyVars GhcPs ->
  -- | Was its head written infix?
  LexicalFixity ->
  -- | Its functional dependencies.
  [LHsFunDep GhcPs] ->
  -- | The signatures and pragmas in its body.
  [LSig GhcPs] ->
  -- | The default definitions of its methods.
  LHsBinds GhcPs ->
  -- | Its associated families.
  [LFamilyDecl GhcPs] ->
  -- | The defaults of those families.
  [LTyFamDefltDecl GhcPs] ->
  -- | The Haddocks among its members.
  [LDocDecl GhcPs] ->
  Doc
classDecl ctx anns ctxt tyCon HsQTvs{..} fixity fdeps sigs binds families defaults docs =
  layoutFrom ctx (wholeHeadSpan <> spansOf members) $
    txt "class" <> layoutFrom ctx wholeHeadSpan head' <> body
  where
    headSpan = spanOf tyCon <> spansOf hsq_explicit
    whereSpan = spanOf (acd_where anns)
    wholeHeadSpan = foldMap spanOf ctxt <> headSpan <> spansOf fdeps

    head' =
      breakOrSpace
        <> indent
          ( foldMap (leftContext ctx headSpan) ctxt
              <> classHead
              <> indent (funDeps ctx fdeps)
              <> includeUnless
                onlyDocs
                (breakOrSpace <> keywordAt whereSpan "where")
          )

    classHead
      | isCTuple (unLoc tyCon) (length hsq_explicit) =
          layoutFrom ctx headSpan
            . layoutWithin ctx (spanOf tyCon) (spansOf hsq_explicit)
            $ parens
              ( insideBrackets
                  (spanOf tyCon)
                  ( commaSep
                      (ctxCommaStyle ctx)
                      (fmap (align . at_ ctx (tyVarBndr ctx)) hsq_explicit)
                  )
              )
      | otherwise =
          typeHead
            ctx
            Outdented
            fixity
            tyCon
            spanOf
            (at_ ctx (tyVarBndr ctx))
            hsq_explicit

    body =
      includeUnless (null members) . indent $
        breakOrSpace <> knotDecls (ctxKnot ctx) ctx Associated members

    onlyDocs = null sigs && null binds && null families && null defaults

    members =
      inSourceOrder
        [ fmap (fmap (SigD NoExtField)) sigs,
          fmap (fmap (ValD NoExtField)) binds,
          fmap (fmap (TyClD NoExtField . FamDecl NoExtField)) families,
          fmap (fmap (InstD NoExtField . TyFamInstD NoExtField)) defaults,
          fmap (fmap (DocD NoExtField)) docs
        ]

-- | Is this the constraint tuple of the given arity?
isCTuple :: RdrName -> Int -> Bool
isCTuple (Exact n) arity = isCTupleTyConName n && n == cTupleTyConName arity
isCTuple _ _ = False

-- | The functional dependencies of a class.
funDeps :: Ctx -> [LHsFunDep GhcPs] -> Doc
funDeps _ [] = mempty
funDeps ctx fdeps =
  breakOrSpace
    <> commaSepAfter
      (ctxCommaStyle ctx)
      "|"
      (fmap (align . at_ ctx (funDep ctx)) fdeps)

-- | One functional dependency of a class, as in @a b -> c@.
funDep :: Ctx -> FunDep GhcPs -> Doc
funDep ctx (FunDep _ before after) =
  sepBy space (fmap (name ctx) before)
    <> space
    <> txt (reserved ctx "->")
    <> space
    <> sepBy space (fmap (name ctx) after)

-- | A class instance.
clsInstDecl ::
  -- | The context.
  Ctx ->
  -- | The instance.
  ClsInstDecl GhcPs ->
  Doc
clsInstDecl ctx ClsInstDecl{cid_ext = (warning, anns, _), ..} =
  txt "instance" <> layoutFrom ctx headSpan head' <> body
  where
    headSpan = foldMap spanOf warning <> typeSpan
    typeSpan = spanOf cid_poly_ty
    whereSpan = spanOf (acid_where anns)
    typeRegion
      | null members = typeSpan <> whereSpan
      | otherwise = typeSpan

    head' =
      breakOrSpace
        <> indent
          ( foldMap (\w -> at ctx w (warningTxt ctx) <> breakOrSpace) warning
              <> foldMap (<> breakOrSpace) (overlapMode ctx cid_overlap_mode)
              <> ( maybe id located typeRegion $
                     layoutFrom ctx typeSpan (hsSigTypeBody ctx (unLoc cid_poly_ty))
                       <> includeUnless
                         (null members)
                         (breakOrSpace <> keywordAt whereSpan "where")
                 )
          )

    body =
      includeUnless (null members) . indent $
        breakOrSpace <> knotDecls (ctxKnot ctx) ctx Associated members

    members =
      inSourceOrder
        [ fmap (fmap (SigD NoExtField)) cid_sigs,
          fmap (fmap (ValD NoExtField)) cid_binds,
          fmap (fmap (InstD NoExtField . TyFamInstD NoExtField)) cid_tyfam_insts,
          fmap (fmap (InstD NoExtField . DataFamInstD NoExtField)) cid_datafam_insts
        ]

-- | A standalone @deriving@ declaration.
standaloneDerivDecl ::
  -- | The context.
  Ctx ->
  -- | The declaration.
  DerivDecl GhcPs ->
  Doc
standaloneDerivDecl ctx DerivDecl{deriv_ext = (warning, _), ..} =
  txt "deriving" <> space <> strategy
  where
    instanceHead =
      txt "instance"
        <> breakOrSpace
        <> indent
          ( foldMap (\w -> at ctx w (warningTxt ctx) <> breakOrSpace) warning
              <> foldMap (<> breakOrSpace) (overlapMode ctx deriv_overlap_mode)
              <> hsSigType ctx (hswc_body deriv_type)
          )

    strategy = case deriv_strategy of
      Nothing -> instanceHead
      Just (L _ s) -> case s of
        StockStrategy _ -> txt "stock " <> instanceHead
        AnyclassStrategy _ -> txt "anyclass " <> instanceHead
        NewtypeStrategy _ -> txt "newtype " <> instanceHead
        ViaStrategy (XViaStrategyPs _ sigTy) ->
          txt "via"
            <> breakOrSpace
            <> indent (hsSigType ctx sigTy)
            <> breakOrSpace
            <> indent instanceHead

-- | A type family instance.
tyFamInstDecl :: Ctx -> FamilyStyle -> TyFamInstDecl GhcPs -> Doc
tyFamInstDecl ctx style TyFamInstDecl{..} =
  txt keyword <> breakOrSpace <> indent (tyFamInstEqn ctx tfid_eqn)
  where
    keyword = case style of
      Associated -> "type"
      Free -> "type instance"

-- | A data family instance.
dataFamInstDecl :: Ctx -> FamilyStyle -> DataFamInstDecl GhcPs -> Doc
dataFamInstDecl ctx style (DataFamInstDecl FamEqn{..}) =
  dataDecl
    ctx
    (fromBool (style == Free))
    feqn_tycon
    feqn_pats
    typeArgSpan
    (typeArgument ctx)
    feqn_fixity
    outerBinders
    feqn_rhs
  where
    -- @data instance forall k (a :: k). D a = …@ binds its variables ahead
    -- of the head, exactly as a type family instance does.
    outerBinders =
      outerBndrs
        ctx
        breakOrSpace
        (spanOf feqn_tycon <> foldMap typeArgSpan feqn_pats)
        feqn_bndrs

-- | A @data family@ or @type family@ declaration.
famDecl ::
  -- | The context.
  Ctx ->
  -- | Does it stand on its own or in a class?
  FamilyStyle ->
  -- | The declaration.
  FamilyDecl GhcPs ->
  Doc
famDecl ctx style FamilyDecl{fdTyVars = HsQTvs{..}, ..} =
  txt keyword <> txt familyWord <> head' <> equations
  where
    (keyword, closedEqns) = case fdInfo of
      DataFamily -> ("data", Nothing)
      OpenTypeFamily -> ("type", Nothing)
      ClosedTypeFamily eqs -> ("type", Just eqs)
    familyWord = case style of
      Associated -> ""
      Free -> " family"

    headSpan = spanOf fdLName <> spansOf hsq_explicit
    headAndSigSpan = spanOf fdResultSig <> headSpan

    head' =
      indent . layoutFrom ctx headAndSigSpan $
        breakOrSpace
          <> typeHead
            ctx
            Outdented
            fdFixity
            fdLName
            spanOf
            (at_ ctx (tyVarBndr ctx))
            hsq_explicit
          <> includeUnless
            (isNothing resultSig && isNothing fdInjectivityAnn)
            space
          <> indent
            ( fold resultSig
                <> foldMap
                  ((space <>) . at_ ctx (injectivityAnn ctx))
                  fdInjectivityAnn
            )
          <> foldMap (const (breakOrSpace <> txt "where")) closedEqns

    resultSig = familyResultSig ctx headSpan fdResultSig

    equations = case closedEqns of
      Nothing -> mempty
      Just eqs ->
        case eqs of
          Nothing -> space <> txt ".."
          Just given ->
            includeUnless (null given) $
              hardBreak
                <> indent
                  ( sepBy
                      hardBreak
                      (fmap (at_ ctx (blockItem . tyFamInstEqn ctx)) given)
                  )

-- | What the head of a family says of its result, a kind or a variable
-- naming it, if anything.
familyResultSig ::
  -- | The context.
  Ctx ->
  -- | The head.
  Maybe Span ->
  -- | What it says.
  LFamilyResultSig GhcPs ->
  Maybe Doc
familyResultSig ctx headSpan (L _ sig) = case sig of
  NoSig NoExtField -> Nothing
  KindSig NoExtField k ->
    Just
      ( joinedByArrow ctx breakOrSpace headSpan (spanOf k) (reserved ctx "::")
          <> hsType ctx k
      )
  TyVarSig NoExtField bndr ->
    Just (txt "=" <> breakOrSpace <> at ctx bndr (tyVarBndr ctx))

-- | The injectivity annotation of a type family, as in @| r -> a@.
injectivityAnn :: Ctx -> InjectivityAnn GhcPs -> Doc
injectivityAnn ctx (InjectivityAnn _ from to) =
  txt "|"
    <> space
    <> name ctx from
    <> space
    <> txt (reserved ctx "->")
    <> space
    <> sepBy space (fmap (name ctx) to)

-- | One equation of a type family.
tyFamInstEqn ::
  -- | The context.
  Ctx ->
  -- | The equation.
  TyFamInstEqn GhcPs ->
  Doc
tyFamInstEqn ctx FamEqn{..} =
  binders <> nest (if hasBinders then 1 else 0) (lhs <> rhs)
  where
    binders = outerBndrs ctx breakOrSpace headSpan feqn_bndrs
    headSpan = spanOf feqn_tycon <> foldMap typeArgSpan feqn_pats
    hasBinders = case feqn_bndrs of
      HsOuterExplicit _ (_ : _) -> True
      _ -> False

    lhs =
      typeHead
        ctx
        Indented
        feqn_fixity
        feqn_tycon
        typeArgSpan
        (typeArgument ctx)
        feqn_pats

    rhs =
      indent $
        space
          <> txt "="
          <> layoutFrom
            ctx
            (fmap endOf headSpan <> spanOf feqn_rhs)
            (breakOrSpace <> hsType ctx feqn_rhs)

-- | A @type role@ declaration.
roleAnnot :: Ctx -> RoleAnnotDecl GhcPs -> Doc
roleAnnot ctx (RoleAnnotDecl _ tyCon roles) =
  txt "type role"
    <> breakOrSpace
    <> indent
      ( name ctx tyCon
          <> breakOrSpace
          <> indent
            (align (sepBy breakOrSpace (fmap (align . at_ ctx role) roles)))
      )
  where
    role = maybe (txt "_") $ \case
      Nominal -> txt "nominal"
      Representational -> txt "representational"
      Phantom -> txt "phantom"
