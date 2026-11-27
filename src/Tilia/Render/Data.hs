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
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, isNothing, listToMaybe, mapMaybe)
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
  -- | Whether @instance@ follows the keyword, as it does in a family
  -- instance that stands on its own.
  Choice "instanceWord" ->
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
  -- | Where the parentheses around the head were written, where there are
  -- any.
  (Maybe Span, Maybe Span) ->
  -- | The @forall@ a family instance may bind its variables with, which an
  -- ordinary declaration does not have and passes as 'mempty'.
  Doc ->
  -- | The rest of the declaration.
  HsDataDefn GhcPs ->
  Doc
dataDecl
  ctx
  instanceWord
  tyCon
  tyVars
  tyVarSpan
  renderTyVar
  fixity
  headParens
  outerBinders
  HsDataDefn{..} =
    keywords <> header <> constructors <> derivings
    where
      keywords =
        refAt
          ( spanOf (andd_type dd_ext)
              <> spanOf (andd_newtype dd_ext)
              <> spanOf (andd_data dd_ext)
              <> spanOf (andd_instance dd_ext)
          )
          (txt (keyword <> if isTrue instanceWord then " instance" else ""))
      keyword = case dd_cons of
        NewTypeCon _ -> "newtype"
        DataTypeCons False _ -> "data"
        DataTypeCons True _ -> "type data"

      headSpan = spanOf tyCon <> foldMap tyVarSpan tyVars
      wholeHeadSpan =
        headSpan
          <> foldMap spanOf dd_kindSig
          <> foldMap spanOf dd_ctxt
          <> foldMap spanOf dd_cType

      header =
        layoutFrom ctx wholeHeadSpan . indent 1 (ptr keywords) $
          foreignType
            <> breakOrSpace
            <> outerBinders
            <> foldMap (leftContext ctx headSpan) dd_ctxt
            <> kinded
              (typeHead ctx 0 headParens fixity tyCon tyVarSpan renderTyVar tyVars)
            <> includeWhen
              (isJust (spanOf (andd_where dd_ext)))
              (breakOrSpace <> txt "where")

      kinded h = case dd_kindSig of
        Just k -> h >+> typeAscription ctx headSpan (asSigType k)
        Nothing -> h

      foreignType = case unLoc <$> dd_cType of
        Nothing -> mempty
        Just (CType _ header' (type_, _)) ->
          breakOrSpace
            <> txt "{-# CTYPE"
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
              indent 1 (ptr keywords) $
                breakOrSpace
                  <> items
                    (if null (drop 1 cons) then NoBrace else MayBrace)
                    (fmap (at_ ctx (conDecl ctx)) cons)
          | otherwise ->
              layoutFrom ctx (fmap endOf wholeHeadSpan <> spansOf cons)
                . indent 1 (ptr keywords)
                $ beforeEquals
                  <> leading (txt "=")
                  <> alternatives
          where
            singleRecCon = case cons of
              [L _ ConDeclH98{con_args = RecCon{}}] -> True
              _ -> False
            compactAroundEquals =
              sameLine
                (fmap endOf wholeHeadSpan)
                (fmap startOf (spanOf firstCon))

            equalsSpan = spanOf (andd_equal dd_ext)

            lineHaddocks = any (endsALine . unLoc) cons
            endsALine = \case
              ConDeclH98{..} ->
                isJust con_doc
                  || not singleRecCon && argumentsDocumented ctx con_args
              ConDeclGADT{} -> False

            beforeEquals
              | lineHaddocks = hardBreak
              | singleRecCon && compactAroundEquals = space
              | otherwise = breakOrSpace

            separator
              | lineHaddocks = hardBreak <> leadingBar
              | otherwise = breakOrSpace <> leadingBar

            keepTogether
              | lineHaddocks || not singleRecCon || not compactAroundEquals =
                  align
              | otherwise = id

            alternatives =
              fence
                (fmap endOf equalsSpan <> spanOf firstCon)
                (keepTogether (constructor firstCon))
                <> mconcat (zipWith alternative cons (drop 1 cons))
            alternative previous c =
              separator
                <> keepTogether
                  (fromItsBar (barAfter previous <> spanOf c) (constructor c))
              where
                fromItsBar
                  | sameLine (spanOf previous) (barAfter previous) = fence
                  | otherwise = maybe id located
            constructor = at_ ctx (conDecl ctx)
      derivings =
        includeUnless (null dd_derivs) beforeDerivings
          <> indent
            1
            (ptr keywords)
            (sepBy hardBreak (fmap (at_ ctx (derivingClause ctx)) dd_derivs))
      beforeDerivings
        | length dd_derivs > 1 = hardBreak
        | otherwise = breakOrSpace

-- | Do the Haddocks of a constructor's arguments or fields take whole
-- lines?
argumentsDocumented ::
  -- | The context.
  Ctx ->
  -- | The arguments or the fields.
  HsConDeclH98Details GhcPs ->
  Bool
argumentsDocumented ctx = \case
  PrefixCon xs -> printsWholeLineDocs ctx (Isn't #under) (mapMaybe cdf_doc xs)
  RecCon l ->
    printsWholeLineDocs
      ctx
      (fieldsUnder ctx)
      (mapMaybe (fieldDoc . unLoc) (unLoc l))
  InfixCon{} -> False

-- | Where the bar after a constructor was written.
barAfter ::
  -- | The constructor.
  LConDecl GhcPs ->
  Maybe Span
barAfter (L l _) =
  listToMaybe [s | AddVbarAnn t <- lann_trailing (anns l), Just s <- [spanOf t]]

-- | Is this a GADT constructor?
isGadtCon :: ConDecl GhcPs -> Bool
isGadtCon = \case
  ConDeclGADT{} -> True
  ConDeclH98{} -> False

-- | One constructor.
conDecl ::
  -- | The context.
  Ctx ->
  -- | The constructor.
  ConDecl GhcPs ->
  Doc
conDecl ctx ConDeclGADT{..} =
  foldMap (haddock ctx Pipe Closed) con_doc
    <> layoutFrom ctx declSpan (brokenForDocs body)
  where
    brokenForDocs = if documented then broken else id
    documented = case con_g_args of
      PrefixConGADT _ _ ->
        printsWholeLineDocs ctx argumentsUnder (con_g_args, con_res_ty)
      RecConGADT _ x ->
        argumentsDocumented ctx (RecCon x)
          || printsWholeLineDocs ctx argumentsUnder con_res_ty
    argumentsUnder = fromBool (ctxArrowStyle ctx == LeadingArrows)

    body =
      ascribedNames ctx (Do #indentTail) con_names sigSpan . brokenForDocs $
        joinedByArrow
          ctx
          breakOrSpace
          (spansOf (NE.toList con_names))
          sigSpan
          (reserved ctx "::")
          <> layoutFrom
            ctx
            sigSpan
            (brokenForDocs signature)

    signature =
      outerBndrs ctx breakOrSpace afterOuter (unLoc con_outer_bndrs)
        <> foldMap (forallTelescope ctx breakOrSpace afterInner) con_inner_bndrs
        <> foldMap (leftContext ctx argResSpan) con_mb_cxt
        <> layoutFrom
          ctx
          argResSpan
          (brokenForDocs argsAndResult)
    afterInner = foldMap spanOf con_mb_cxt <> argResSpan
    afterOuter = foldMap telescopeSpan con_inner_bndrs <> afterInner
    telescopeSpan = \case
      HsForAllInvis _ bndrs -> spansOf bndrs
      HsForAllVis _ bndrs -> spansOf bndrs

    argsAndResult = arguments <> resultType

    -- GHC keeps a GADT's result type without the brackets it was written
    -- with, and three shapes need them back: a kind signature, as
    -- @MkT :: Int -> T :: Star@ does not parse, and a @forall@ or a context
    -- making up the whole signature, which would be read as the
    -- constructor's own.
    resultType = case unLoc con_res_ty of
      HsKindSig{} -> parens (Nothing, Nothing) (hsType ctx con_res_ty)
      HsForAllTy{}
        | standsAlone -> parens (Nothing, Nothing) (hsType ctx con_res_ty)
      HsQualTy{}
        | standsAlone -> parens (Nothing, Nothing) (hsType ctx con_res_ty)
      _ -> signaturePart ctx (hsType ctx) con_res_ty
    standsAlone = case (unLoc con_outer_bndrs, con_g_args) of
      (HsOuterImplicit{}, PrefixConGADT _ []) ->
        null con_inner_bndrs && null con_mb_cxt
      _ -> False
    arguments = case con_g_args of
      PrefixConGADT NoExtField xs ->
        mconcat
          ( zipWith
              argument
              xs
              (fmap (spanOf . cdf_type) (drop 1 xs) <> [spanOf con_res_ty])
          )
      RecConGADT _ x ->
        recordFieldsAt ctx x
          <> joinedByArrow
            ctx
            breakOrSpace
            (spanOf x)
            (spanOf con_res_ty)
            (reserved ctx "->")
    argument x next =
      documentedArgument x
        <> joinedByFunctionArrow
          ctx
          (hsType ctx)
          breakOrSpace
          (spanOf (cdf_type x))
          next
          (cdf_multiplicity x)
    documentedArgument x = case ctxArrowStyle ctx of
      TrailingArrows -> documentedConDeclField ctx x
      LeadingArrows ->
        conDeclField ctx x
          <> foldMap (\d -> hardBreak <> haddock ctx Caret Open d) (cdf_doc x)

    declSpan = spansOf (NE.toList con_names) <> sigSpan
    sigSpan = spanOf con_outer_bndrs <> foldMap spanOf con_mb_cxt <> argResSpan
    argResSpan =
      spanOf con_res_ty <> case con_g_args of
        PrefixConGADT NoExtField xs -> spansOf (fmap cdf_type xs)
        RecConGADT _ x -> spanOf x
conDecl ctx ConDeclH98{..} = case con_args of
  PrefixCon xs ->
    ownDoc
      <> existentials
        ( brokenForDocs $
            if null xs
              then name ctx con_name
              else
                name ctx con_name
                  >+> breakOrSpace
                    <> align
                      ( sepBy
                          breakOrSpace
                          (fmap (align . documentedConDeclField ctx) xs)
                      )
        )
  RecCon l ->
    ownDoc
      <> existentials
        ( brokenForDocs $
            name ctx con_name >+> breakOrNothing <> recordFieldsAt ctx l
        )
  InfixCon l r ->
    includeWhen docOnTop ownDoc
      <> existentials
        ( leftArgument l
            >+> includeUnless docOnTop ownDoc
              <> name ctx con_name
              <> rightDoc r
              <> conDeclField ctx r
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
                Just d -> space <> haddockInline ctx Caret d
                Nothing -> breakOrSpace
      rightDoc x = case cdf_doc x of
        Just d -> hardBreak <> haddock ctx Pipe Closed d
        Nothing -> breakOrSpace
  where
    brokenForDocs = if argumentsDocumented ctx con_args then broken else id

    ownDoc = foldMap (haddock ctx Pipe Closed) con_doc

    existentials body
      | con_forall || isJust con_mb_cxt =
          layoutFrom ctx contextSpan $
            includeWhen
              con_forall
              ( quantifier
                  ctx
                  breakOrSpace
                  Invisible
                  (maybe (spanOf con_name) spanOf con_mb_cxt)
                  con_ex_tvs
              )
              <> foldMap (leftContext ctx (spanOf con_name)) con_mb_cxt
              <> variant laidOut (align laidOut)
      | otherwise = laidOut
      where
        laidOut = layoutFrom ctx declSpan body

    contextSpan =
      spanOfSrcSpan (getHasLoc (acdh_forall con_ext))
        <> spansOf con_ex_tvs
        <> foldMap spanOf con_mb_cxt
        <> fmap startOf (spanOf con_name <> leftOperand)
    leftOperand = case con_args of
      InfixCon l _ -> spanOf (cdf_type l)
      _ -> Nothing

    declSpan = spanOf con_name <> argSpans
    argSpans = case con_args of
      PrefixCon xs -> spansOf (fmap cdf_type xs)
      RecCon l -> spanOf l
      InfixCon x y -> spansOf (fmap cdf_type [x, y])

-- | One @deriving@ clause, with the strategy it was written with.
derivingClause ::
  -- | The context.
  Ctx ->
  -- | The clause.
  HsDerivingClause GhcPs ->
  Doc
derivingClause ctx HsDerivingClause{..} =
  brokenForDocs $ keyword <> space <> strategy
  where
    brokenForDocs = if documented then broken else id
    documented = printsWholeLineDocs ctx (Isn't #under) deriv_clause_tys
    what =
      at ctx deriv_clause_tys $ \tys ->
        brokenForDocs $ case tys of
          DctSingle NoExtField sigTy ->
            parens (contextParens deriv_clause_tys) (hsSigType ctx sigTy)
          DctMulti NoExtField sigTys ->
            parens (contextParens deriv_clause_tys)
              . insideBrackets (spanOf deriv_clause_tys)
              $ commaSep
                (ctxCommaStyle ctx)
                (fmap (align . hsSigType ctx) sigTys)

    keyword = refAt (spanOf deriv_clause_ext) (txt "deriving")
    strategy = case deriv_clause_strategy of
      Nothing -> breakOrSpace <> indent 1 (ptr keyword) what
      Just (L _ s) -> case s of
        StockStrategy tok -> named tok "stock"
        AnyclassStrategy tok -> named tok "anyclass"
        NewtypeStrategy tok -> named tok "newtype"
        ViaStrategy (XViaStrategyPs tok sigTy) ->
          breakOrSpace
            <> indent
              1
              (ptr keyword)
              ( what
                  <> breakOrSpace
                  <> keywordAt (spanOf tok) "via"
                  <> space
                  <> hsSigType ctx sigTy
              )
      where
        named tok kw =
          indent 1 (ptr keyword) (keywordAt (spanOf tok) kw)
            <> breakOrSpace
            <> indent 1 (ptr keyword) what

-- | @type T a = …@.
synDecl ::
  -- | The context.
  Ctx ->
  -- | Where its keyword and the parentheses around its head were.
  AnnSynDecl ->
  -- | The synonym's name.
  LocatedN RdrName ->
  -- | Was its head written infix?
  LexicalFixity ->
  -- | Its type variables.
  LHsQTyVars GhcPs ->
  -- | What it stands for.
  LHsType GhcPs ->
  Doc
synDecl ctx written tyCon fixity HsQTvs{..} rhs =
  keyword
    <> indent
      1
      (ptr keyword)
      ( space
          <> layoutFrom
            ctx
            headSpan
            ( defHead
                0
                (outerBracketSpans (asd_opens written) (asd_closes written))
                (fromBool (fixity == Infix))
                (name ctx tyCon)
                (fmap (at_ ctx (tyVarBndr ctx)) hsq_explicit)
            )
      )
    <> indent
      1
      (ptr keyword)
      ( trailing (txt "=")
          <> layoutFrom
            ctx
            (fmap endOf headSpan <> spanOf rhs)
            (separator <> hsType ctx rhs)
      )
  where
    keyword = refAt (spanOf (asd_type written)) (txt "type")
    headSpan = spanOf tyCon <> spansOf hsq_explicit
    separator = typeBreak (typeIsDocumented (unLoc rhs))
