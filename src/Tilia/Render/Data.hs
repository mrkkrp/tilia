{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data types and type synonyms.
module Tilia.Render.Data
  ( dataDecl,
    synDecl,
  )
where

import Data.Choice (Choice, fromBool, isTrue, pattern Do, pattern Isn't)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, isNothing, mapMaybe, maybeToList)
import GHC.Hs
import GHC.Types.Fixity (LexicalFixity (..))
import GHC.Types.ForeignCall (CType (..), Header (..))
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Haddock
import Tilia.Render.Layout
import Tilia.Render.Name
import Tilia.Render.Type
import Tilia.Span
import Tilia.Span.Ghc

-- | A @data@, @newtype@ or @type data@ declaration, or an instance of one.
--
-- The type variables are left abstract because a data instance is applied
-- to types rather than to variables, and the two are otherwise printed
-- identically.
dataDecl ::
  -- | The context.
  Ctx ->
  -- | The family style.
  FamilyStyle ->
  -- | The type constructor.
  LocatedN RdrName ->
  -- | What it is applied to.
  [tyVar] ->
  -- | Where each of those was.
  (tyVar -> Maybe Span) ->
  -- | How to print one.
  (tyVar -> Doc) ->
  -- | Was the head written infix?
  LexicalFixity ->
  -- | The @forall@ a family instance may bind its variables with, which an
  -- ordinary declaration does not have and passes as 'mempty'.
  Doc ->
  HsDataDefn GhcPs ->
  Doc
dataDecl ctx style tyCon tyVars tyVarSpan renderTyVar fixity outerBinders HsDataDefn {..} =
  txt keyword <> txt instanceWord <> header <> constructors <> derivings
  where
    keyword = case dd_cons of
      NewTypeCon _ -> "newtype"
      DataTypeCons False _ -> "data"
      DataTypeCons True _ -> "type data"

    instanceWord = case style of
      Associated -> ""
      Free -> " instance"

    headSpan = spanOf tyCon <> foldMap tyVarSpan tyVars
    wholeHeadSpan =
      headSpan
        <> foldMap spanOf dd_kindSig
        <> foldMap spanOf dd_ctxt
        <> foldMap spanOf dd_cType

    header =
      layoutFrom ctx wholeHeadSpan . indent $
        foreignType
          <> breakOrSpace
          <> outerBinders
          <> foldMap (leftContext ctx) dd_ctxt
          <> layoutFrom
            ctx
            headSpan
            ( defHead
                (fromBool (fixity == Infix))
                (Do #indentArgs)
                (name ctx tyCon)
                (fmap renderTyVar tyVars)
            )
          <> foldMap kindSignature dd_kindSig

    kindSignature k =
      joinedBy "::" <> indent (hsType ctx k)

    foreignType = case unLoc <$> dd_cType of
      Nothing -> mempty
      Just (CType prag header' (type_, _)) ->
        breakOrSpace
          <> sourceText prag
          <> foldMap (\(Header h _) -> space <> sourceText h) header'
          <> space
          <> sourceText type_
          <> txt " #-}"

    cons = case dd_cons of
      NewTypeCon c -> [c]
      DataTypeCons _ cs -> cs

    isGadt = isJust dd_kindSig || any (isGadtCon . unLoc) cons

    constructors = case cons of
      [] -> mempty
      (firstCon : _)
        | isGadt ->
            indent $
              layoutFrom ctx wholeHeadSpan (breakOrSpace <> txt "where")
                <> breakOrSpace
                <> items
                  (if null (drop 1 cons) then NoBrace else MayBrace)
                  (fmap (at_ ctx (conDecl ctx (Isn't #singleRecCon))) cons)
        | otherwise ->
            layoutFrom ctx (spanOf tyCon <> spansOf cons) . indent $
              beforeEquals <> txt "=" <> space <> alternatives
        where
          singleRecCon = case cons of
            [L _ ConDeclH98 {con_args = RecCon {}}] -> True
            _ -> False
          compactAroundEquals =
            sameLine (spanOf tyCon) (conNamesSpan (unLoc firstCon))
          conNamesSpan = \case
            ConDeclGADT {..} -> spansOf (NE.toList con_names)
            ConDeclH98 {..} -> spanOf con_name

          lineHaddocks = any (printsWholeLineDocs ctx . visibleDocs . unLoc) cons

          beforeEquals
            | lineHaddocks = hardBreak
            | singleRecCon && compactAroundEquals = space
            | otherwise = breakOrSpace

          separator
            | lineHaddocks = hardBreak <> txt "|" <> space
            | otherwise = breakOrSpace <> txt "|" <> space

          keepTogether
            | lineHaddocks || not singleRecCon = align
            | otherwise = id

          alternatives =
            sepBy
              separator
              ( fmap
                  (keepTogether . at_ ctx (conDecl ctx (fromBool singleRecCon)))
                  cons
              )
    derivings =
      includeUnless (null dd_derivs) beforeDerivings
        <> indent (vsep (fmap (at_ ctx (derivingClause ctx)) dd_derivs))
    beforeDerivings
      | length dd_derivs > 1 = hardBreak
      | otherwise = breakOrSpace

-- | The documentation a constructor's own layout has to make room for.
visibleDocs :: ConDecl GhcPs -> [LHsDoc GhcPs]
visibleDocs = \case
  ConDeclH98 {..} ->
    maybeToList con_doc <> case con_args of
      PrefixCon xs -> mapMaybe cdf_doc xs
      _ -> []
  ConDeclGADT {} -> []

-- | Is this a GADT constructor?
isGadtCon :: ConDecl GhcPs -> Bool
isGadtCon = \case
  ConDeclGADT {} -> True
  ConDeclH98 {} -> False

-- | One constructor.
conDecl :: Ctx -> Choice "singleRecCon" -> ConDecl GhcPs -> Doc
conDecl ctx _ ConDeclGADT {..} =
  foldMap (haddock ctx Pipe Closed) con_doc
    <> layoutFrom ctx declSpan (brokenIfDocumented ctx documented body)
  where
    documented = (con_g_args, con_res_ty)

    c :| cs = con_names
    body =
      name ctx c
        <> includeUnless
          (null cs)
          (indent (comma <> breakOrSpace <> commaSep (fmap (name ctx) cs)))
        <> joinedBy "::"
        <> indent (layoutFrom ctx sigSpan (brokenIfDocumented ctx documented signature))

    signature =
      outerBndrs ctx (unLoc con_outer_bndrs)
        <> ( case unLoc con_outer_bndrs of
               HsOuterImplicit {} -> mempty
               HsOuterExplicit {} -> breakOrSpace
           )
        <> foldMap (\tele -> forallTelescope ctx tele <> breakOrSpace) con_inner_bndrs
        <> foldMap
          (\qs -> context ctx qs <> joinedBy "=>")
          con_mb_cxt
        <> layoutFrom ctx argResSpan (brokenIfDocumented ctx documented argsAndResult)

    argsAndResult = arguments <> resultType

    -- GHC keeps a GADT's result type without the brackets it was written
    -- with, and there is one shape that does not survive losing them. A
    -- kind signature needs them back: @MkT :: Int -> T :: Star@ reads as a
    -- second signature on the constructor rather than as a kind on its
    -- result, and does not parse at all.
    resultType = case unLoc con_res_ty of
      HsKindSig {} -> parens (hsType ctx con_res_ty)
      HsForAllTy {} | standsAlone -> parens (hsType ctx con_res_ty)
      HsQualTy {} | standsAlone -> parens (hsType ctx con_res_ty)
      _ -> hsType ctx con_res_ty
    standsAlone = case (unLoc con_outer_bndrs, con_g_args) of
      (HsOuterImplicit {}, PrefixConGADT _ []) ->
        null con_inner_bndrs && null con_mb_cxt
      _ -> False
    arguments = case con_g_args of
      PrefixConGADT NoExtField xs -> foldMap argument xs
      RecConGADT _ x ->
        recordFieldsAt ctx x <> joinedBy "->"
    argument x =
      documentedConDeclField ctx x
        <> space
        <> multiplicity (hsType ctx) (cdf_multiplicity x)
        <> joinedBy "->"

    declSpan = spansOf (NE.toList con_names) <> sigSpan
    sigSpan = spanOf con_outer_bndrs <> foldMap spanOf con_mb_cxt <> argResSpan
    argResSpan =
      spanOf con_res_ty <> case con_g_args of
        PrefixConGADT NoExtField xs -> spansOf (fmap cdf_type xs)
        RecConGADT _ x -> spanOf x
conDecl ctx singleRecCon ConDeclH98 {..} = case con_args of
  PrefixCon xs ->
    ownDoc
      <> existentials
      <> layoutFrom
        ctx
        declSpan
        ( brokenIfDocumented ctx xs $
            name ctx con_name
              <> includeUnless (null xs) breakOrSpace
              <> indent (align (sepBy breakOrSpace (fmap (align . documentedConDeclField ctx) xs)))
        )
  RecCon l ->
    ownDoc
      <> existentials
      <> layoutFrom
        ctx
        declSpan
        ( name ctx con_name
            <> breakOrSpace
            <> nest (if isTrue singleRecCon then 0 else 1) (recordFieldsAt ctx l)
        )
  InfixCon l r ->
    includeWhen docOnTop ownDoc
      <> existentials
      <> layoutFrom
        ctx
        declSpan
        ( leftArgument l
            <> indent
              ( includeUnless docOnTop ownDoc
                  <> name ctx con_name
                  <> rightDoc r
                  <> conDeclField ctx r
              )
        )
    where
      docOnTop = isNothing (cdf_doc l) && isNothing (cdf_doc r)
      leftArgument x
        | isJust con_doc =
            foldMap (haddock ctx Pipe Closed) (cdf_doc x)
              <> conDeclField ctx x
              <> breakOrSpace
        | otherwise =
            conDeclField ctx x
              <> case cdf_doc x of
                Just d -> space <> haddock ctx Caret Closed d
                Nothing -> breakOrSpace
      rightDoc x = case cdf_doc x of
        Just d -> hardBreak <> haddock ctx Pipe Closed d
        Nothing -> breakOrSpace
  where
    ownDoc = foldMap (haddock ctx Pipe Closed) con_doc

    existentials =
      layoutFrom ctx contextSpan $
        includeWhen
          con_forall
          (forallBndrs ctx Invisible (tyVarBndr ctx) con_ex_tvs <> breakOrSpace)
          <> foldMap (leftContext ctx) con_mb_cxt

    contextSpan =
      spanOfSrcSpan (getHasLoc (acdh_forall con_ext))
        <> spansOf con_ex_tvs
        <> foldMap spanOf con_mb_cxt
        <> spanOf con_name

    declSpan = spanOf con_name <> argSpans
    argSpans = case con_args of
      PrefixCon xs -> spansOf (fmap cdf_type xs)
      RecCon l -> spanOf l
      InfixCon x y -> spansOf (fmap cdf_type [x, y])

-- | A context standing to the left of a @=>@, with the arrow and the break
-- after it.
leftContext :: Ctx -> LHsContext GhcPs -> Doc
leftContext ctx = \case
  L _ [] -> mempty
  ctxt -> context ctx ctxt <> joinedBy "=>"

-- | One @deriving@ clause, with the strategy it was written with.
derivingClause :: Ctx -> HsDerivingClause GhcPs -> Doc
derivingClause ctx HsDerivingClause {..} =
  brokenIfDocumented ctx deriv_clause_tys $
    txt "deriving" <> space <> strategy
  where
    what =
      at ctx deriv_clause_tys $ \tys ->
        brokenIfDocumented ctx tys $ case tys of
          DctSingle NoExtField sigTy -> parens (hsSigType ctx sigTy)
          DctMulti NoExtField sigTys ->
            parens (commaSep (fmap (align . hsSigType ctx) sigTys))

    strategy = case deriv_clause_strategy of
      Nothing -> breakOrSpace <> indent what
      Just (L _ s) -> case s of
        StockStrategy _ -> named "stock"
        AnyclassStrategy _ -> named "anyclass"
        NewtypeStrategy _ -> named "newtype"
        ViaStrategy (XViaStrategyPs _ sigTy) ->
          breakOrSpace
            <> indent
              ( what
                  <> breakOrSpace
                  <> txt "via"
                  <> space
                  <> hsSigType ctx sigTy
              )
      where
        named kw = txt kw <> breakOrSpace <> indent what

-- | @type T a = …@.
synDecl ::
  Ctx ->
  LocatedN RdrName ->
  LexicalFixity ->
  LHsQTyVars GhcPs ->
  LHsType GhcPs ->
  Doc
synDecl ctx tyCon fixity HsQTvs {..} rhs =
  txt "type"
    <> space
    <> layoutFrom
      ctx
      (spanOf tyCon <> spansOf hsq_explicit)
      ( defHead
          (fromBool (fixity == Infix))
          (Do #indentArgs)
          (name ctx tyCon)
          (fmap (at_ ctx (tyVarBndr ctx)) hsq_explicit)
      )
    <> indent (space <> txt "=" <> separator <> hsType ctx rhs)
  where
    separator
      | typeIsDocumented (unLoc rhs) = hardBreak
      | otherwise = breakOrSpace
