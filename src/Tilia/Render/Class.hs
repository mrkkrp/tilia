{-# LANGUAGE DataKinds #-}
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

import Data.Choice (fromBool, pattern Do)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (isNothing)
import GHC.Builtin.Types (cTupleTyConName, isCTupleTyConName)
import GHC.Core.Coercion.Axiom (Role (..))
import GHC.Hs
import GHC.Types.Fixity (LexicalFixity (..))
import GHC.Types.Name.Reader (RdrName (..))
import GHC.Types.SrcLoc (GenLocated (..), leftmost_smallest, unLoc)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Data (dataDecl)
import Tilia.Render.Name
import Tilia.Render.Pragma
import Tilia.Render.Type
import Tilia.Span.Ghc

-- | A type class declaration.
classDecl ::
  Ctx ->
  AnnClassDecl ->
  Maybe (LHsContext GhcPs) ->
  LocatedN RdrName ->
  LHsQTyVars GhcPs ->
  LexicalFixity ->
  [LHsFunDep GhcPs] ->
  [LSig GhcPs] ->
  LHsBinds GhcPs ->
  [LFamilyDecl GhcPs] ->
  [LTyFamDefltDecl GhcPs] ->
  [LDocDecl GhcPs] ->
  Doc
classDecl ctx anns ctxt tyCon HsQTvs {..} fixity fdeps sigs binds families defaults docs =
  txt "class" <> layoutFrom ctx wholeHeadSpan head' <> body
  where
    headSpan = spanOf tyCon <> spansOf hsq_explicit
    whereSpan = tokenSpan (acd_where anns)
    wholeHeadSpan = foldMap spanOf ctxt <> headSpan <> spansOf fdeps

    head' =
      breakOrSpace
        <> indent
          ( foldMap (classContext ctx) ctxt
              <> layoutFrom ctx headSpan classHead
              <> indent (funDeps ctx fdeps)
              <> includeUnless
                (null members)
                (breakOrSpace <> keywordAt ctx whereSpan "where")
          )

    classHead
      | isCTuple (unLoc tyCon) (length hsq_explicit) =
          layoutWithin ctx (spanOf tyCon) (spansOf hsq_explicit) $
            parens
              ( insideBrackets
                  (spanOf tyCon)
                  (commaSep (fmap (align . at_ ctx (tyVarBndr ctx)) hsq_explicit))
              )
      | otherwise =
          defHead
            (fromBool (fixity == Infix))
            (Do #indentArgs)
            (name ctx tyCon)
            (fmap (at_ ctx (tyVarBndr ctx)) hsq_explicit)

    body =
      includeUnless
        (null members)
        (breakOrSpace <> indent (knotDeclsGrouped (ctxKnot ctx) ctx Associated members))

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

-- | A context on a class head, with the @=>@ that follows it.
classContext :: Ctx -> LHsContext GhcPs -> Doc
classContext ctx ctxt
  | null (unLoc ctxt) = mempty
  | otherwise = context ctx ctxt <> joinedBy "=>"

-- | The functional dependencies of a class.
funDeps :: Ctx -> [LHsFunDep GhcPs] -> Doc
funDeps _ [] = mempty
funDeps ctx fdeps =
  breakOrSpace
    <> txt "|"
    <> space
    <> indent (commaSep (fmap (align . at_ ctx (funDep ctx)) fdeps))

funDep :: Ctx -> FunDep GhcPs -> Doc
funDep ctx (FunDep _ before after) =
  hsep (fmap (name ctx) before)
    <> space
    <> txt "->"
    <> space
    <> hsep (fmap (name ctx) after)

-- | A class instance.
clsInstDecl :: Ctx -> ClsInstDecl GhcPs -> Doc
clsInstDecl ctx ClsInstDecl {cid_ext = (warning, anns, _), ..} =
  txt "instance" <> layoutFrom ctx headSpan head' <> body
  where
    headSpan = foldMap spanOf warning <> spanOf cid_poly_ty
    whereSpan = tokenSpan (acid_where anns)

    head' =
      foldMap (\w -> breakOrSpace <> at ctx w warningTxt) warning
        <> breakOrSpace
        <> at
          ctx
          cid_poly_ty
          ( \sigTy ->
              indent $
                foldMap (<> breakOrSpace) (overlapMode cid_overlap_mode)
                  <> hsSigTypeBody ctx sigTy
                  <> includeUnless
                    (null members)
                    (breakOrSpace <> keywordAt ctx whereSpan "where")
          )

    body =
      includeUnless (null members) . indent $
        breakOrSpace <> knotDeclsGrouped (ctxKnot ctx) ctx Associated members

    members =
      inSourceOrder
        [ fmap (fmap (SigD NoExtField)) cid_sigs,
          fmap (fmap (ValD NoExtField)) cid_binds,
          fmap (fmap (InstD NoExtField . TyFamInstD NoExtField)) cid_tyfam_insts,
          fmap (fmap (InstD NoExtField . DataFamInstD NoExtField)) cid_datafam_insts
        ]

-- | A standalone @deriving@ declaration.
standaloneDerivDecl :: Ctx -> DerivDecl GhcPs -> Doc
standaloneDerivDecl ctx DerivDecl {deriv_ext = (warning, _), ..} =
  txt "deriving" <> space <> strategy
  where
    instanceHead indented =
      indent $
        txt "instance"
          <> foldMap (\w -> breakOrSpace <> at ctx w warningTxt) warning
          <> breakOrSpace
          <> foldMap (<> breakOrSpace) (overlapMode deriv_overlap_mode)
          <> nest (if indented then 1 else 0) (hsSigType ctx (hswc_body deriv_type))

    strategy = case deriv_strategy of
      Nothing -> instanceHead False
      Just (L _ s) -> case s of
        StockStrategy _ -> txt "stock " <> instanceHead False
        AnyclassStrategy _ -> txt "anyclass " <> instanceHead False
        NewtypeStrategy _ -> txt "newtype " <> instanceHead False
        ViaStrategy (XViaStrategyPs _ sigTy) ->
          txt "via"
            <> breakOrSpace
            <> indent (hsSigType ctx sigTy)
            <> breakOrSpace
            <> instanceHead True

-- | A type family instance.
tyFamInstDecl :: Ctx -> FamilyStyle -> TyFamInstDecl GhcPs -> Doc
tyFamInstDecl ctx style TyFamInstDecl {..} =
  txt keyword <> breakOrSpace <> indent (tyFamInstEqn ctx tfid_eqn)
  where
    keyword = case style of
      Associated -> "type"
      Free -> "type instance"

-- | A data family instance.
dataFamInstDecl :: Ctx -> FamilyStyle -> DataFamInstDecl GhcPs -> Doc
dataFamInstDecl ctx style (DataFamInstDecl FamEqn {..}) =
  dataDecl
    ctx
    style
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
    outerBinders = case feqn_bndrs of
      HsOuterImplicit NoExtField -> mempty
      HsOuterExplicit _ bndrs ->
        forallBndrs ctx Invisible (tyVarBndr ctx) bndrs <> breakOrSpace

-- | A @data family@ or @type family@ declaration.
famDecl :: Ctx -> FamilyStyle -> FamilyDecl GhcPs -> Doc
famDecl ctx style FamilyDecl {fdTyVars = HsQTvs {..}, ..} =
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
          <> layoutFrom
            ctx
            headSpan
            ( defHead
                (fromBool (fdFixity == Infix))
                (Do #indentArgs)
                (name ctx fdLName)
                (fmap (at_ ctx (tyVarBndr ctx)) hsq_explicit)
            )
          <> includeUnless
            (isNothing resultSig && isNothing fdInjectivityAnn)
            space
          <> indent
            ( sequence_' resultSig
                <> space
                <> foldMap (at_ ctx (injectivityAnn ctx)) fdInjectivityAnn
            )

    sequence_' = maybe mempty id
    resultSig = familyResultSig ctx fdResultSig

    equations = case closedEqns of
      Nothing -> mempty
      Just eqs ->
        indent (layoutFrom ctx headAndSigSpan (breakOrSpace <> txt "where"))
          <> case eqs of
            Nothing -> space <> txt ".."
            Just given ->
              includeUnless (null given) $
                hardBreak <> indent (vsep (fmap (at_ ctx (tyFamInstEqn ctx)) given))

familyResultSig :: Ctx -> LFamilyResultSig GhcPs -> Maybe Doc
familyResultSig ctx (L _ sig) = case sig of
  NoSig NoExtField -> Nothing
  KindSig NoExtField k ->
    Just (txt "::" <> breakOrSpace <> hsType ctx k)
  TyVarSig NoExtField bndr ->
    Just (txt "=" <> breakOrSpace <> at ctx bndr (tyVarBndr ctx))

injectivityAnn :: Ctx -> InjectivityAnn GhcPs -> Doc
injectivityAnn ctx (InjectivityAnn _ from to) =
  txt "|"
    <> space
    <> name ctx from
    <> space
    <> txt "->"
    <> space
    <> hsep (fmap (name ctx) to)

-- | One equation of a type family.
tyFamInstEqn :: Ctx -> TyFamInstEqn GhcPs -> Doc
tyFamInstEqn ctx FamEqn {..} =
  binders <> nest (if hasBinders then 1 else 0) (lhs <> rhs)
  where
    (binders, hasBinders) = case feqn_bndrs of
      HsOuterImplicit NoExtField -> (mempty, False)
      HsOuterExplicit _ bndrs ->
        ( forallBndrs ctx Invisible (tyVarBndr ctx) bndrs <> breakOrSpace,
          not (null bndrs)
        )

    lhs =
      layoutFrom ctx (spanOf feqn_tycon <> foldMap typeArgSpan feqn_pats) $
        defHead
          (fromBool (feqn_fixity == Infix))
          (Do #indentArgs)
          (name ctx feqn_tycon)
          (fmap (typeArgument ctx) feqn_pats)

    rhs =
      indent (joinedBy "=" <> hsType ctx feqn_rhs)

-- | A @type role@ declaration.
roleAnnot :: Ctx -> RoleAnnotDecl GhcPs -> Doc
roleAnnot ctx (RoleAnnotDecl _ tyCon roles) =
  txt "type role"
    <> breakOrSpace
    <> indent
      ( name ctx tyCon
          <> breakOrSpace
          <> indent (align (sepBy breakOrSpace (fmap (align . at_ ctx role) roles)))
      )
  where
    role = maybe (txt "_") $ \case
      Nominal -> txt "nominal"
      Representational -> txt "representational"
      Phantom -> txt "phantom"

-- | Merge several lists of declarations back into the order they were
-- written in.
inSourceOrder :: [[LHsDecl GhcPs]] -> [LHsDecl GhcPs]
inSourceOrder = sortBy (leftmost_smallest `on` getLocA) . concat
