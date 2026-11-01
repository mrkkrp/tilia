{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Patterns.
module Tilia.Render.Pattern
  ( hsPat,
    hsPatClosing,
    fieldOcc,
    unboxedSum,
    boxityBrackets,
  )
where

import Data.Choice (Choice, fromBool, isTrue, pattern Is, pattern Isn't)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import GHC.Hs
import GHC.Types.Basic (Arity, Boxity (..), ConTag)
import GHC.Types.Name.Reader (RdrName)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Layout
import Tilia.Render.Literal (hsLit)
import Tilia.Render.Name
import Tilia.Render.Type
import Tilia.Span
import Tilia.Span.Ghc

-- | A pattern.
hsPat :: Ctx -> LPat GhcPs -> Doc
hsPat ctx = hsPatClosing ctx Indented

-- | A pattern whose outermost bracket closes where it is told to.
hsPatClosing :: Ctx -> ClosingIndent -> LPat GhcPs -> Doc
hsPatClosing ctx = hsPatIn ctx (Isn't #inParens)

-- | A pattern that knows where it stands.
hsPatIn ::
  -- | The context.
  Ctx ->
  -- | Is this inside parentheses?
  Choice "inParens" ->
  -- | Where its outermost bracket closes.
  ClosingIndent ->
  -- | The pattern.
  LPat GhcPs ->
  Doc
hsPatIn ctx inParens closing l =
  at ctx l (patBody ctx inParens closing (spanOf l))

-- | Every shape a pattern can take.
patBody ::
  -- | The context.
  Ctx ->
  -- | Is this inside parentheses?
  Choice "inParens" ->
  -- | Where its outermost bracket closes.
  ClosingIndent ->
  -- | Where the pattern was written.
  Maybe Span ->
  -- | The pattern.
  Pat GhcPs ->
  Doc
patBody ctx inParens closing here = \case
  WildPat _ -> txt "_"
  VarPat _ n -> name ctx n
  LazyPat _ p -> txt "~" <> recur p
  AsPat _ n p -> name ctx n <> txt "@" <> recur p
  ParPat _ p ->
    parensWith
      closing
      (insideBrackets here (hsPatIn ctx (Is #inParens) Indented p))
  BangPat _ p -> txt "!" <> recur p
  ListPat _ ps ->
    bracketsWith closing (insideBrackets here (commaSep (fmap recur ps)))
  TuplePat _ ps boxity ->
    boxityBrackets
      boxity
      closing
      (insideBrackets here (commaSep (fmap (align . recur) ps)))
  OrPat _ ps ->
    itemsSepBy (fromBool (isTrue inParens)) NoBrace (fmap recur (NE.toList ps))
  SumPat _ p tag arity -> unboxedSum closing tag arity (recur p)
  ConPat _ con details -> conPattern ctx inParens here con details
  ViewPat _ e p ->
    align $
      knotExpr (ctxKnot ctx) ctx plainSite e
        <> joinedBy (reserved ctx "->")
        <> indent (recur p)
  SplicePat _ splice -> knotSplice (ctxKnot ctx) ctx DollarSplice splice
  LitPat _ lit -> hsLit lit
  NPat _ v (isJust -> negated) _ ->
    includeWhen negated (txt "-" <> negativeLiteralGap ctx)
      <> at ctx v (outputable . ol_val)
  NPlusKPat _ n k _ _ _ ->
    align $
      name ctx n
        <> breakOrSpace
        <> indent (txt "+" <> space <> at ctx k (outputable . ol_val))
  SigPat _ p HsPS{..} ->
    recur p <> typeAscription ctx (asSigType hsps_body)
  EmbTyPat _ (HsTP _ ty) -> txt "type" <> space <> hsType ctx ty
  InvisPat _ (HsTP _ ty) -> txt "@" <> hsType ctx ty
  where
    recur = hsPatIn ctx inParens Indented

-- | A constructor pattern, in whichever of its three forms.
conPattern ::
  -- | The context.
  Ctx ->
  -- | Is this inside parentheses?
  Choice "inParens" ->
  -- | Where the pattern was written.
  Maybe Span ->
  -- | The constructor.
  LocatedN RdrName ->
  -- | What it was applied to.
  HsConPatDetails GhcPs ->
  Doc
conPattern ctx inParens here con = \case
  PrefixCon args ->
    align $
      name ctx con
        <> includeUnless (null args) breakOrSpace
        <> indent (align (sepBy breakOrSpace (fmap (align . recur) args)))
  RecCon (HsRecFields _ fields dotdot) ->
    name ctx con
      <> breakOrNothing
      <> indent (braces (insideBrackets here (commaSep (fmap field (visibleFields dotdot fields)))))
  InfixCon l r ->
    layoutFrom ctx (spanOf l <> spanOf r) $
      recur l
        <> attachOperator Normal (spanOf l) (spanOf con) (spanOf r) (name ctx con) (recur r)
  where
    recur = hsPatIn ctx inParens Indented
    field = either wildcard (at_ ctx (patFieldBind ctx))
    wildcard l = at ctx l (const (txt ".."))
    visibleFields dotdot fields = case dotdot of
      Nothing -> Right <$> fields
      Just l -> (Right <$> fields) <> [Left l]

-- | One field of a record pattern.
patFieldBind :: Ctx -> HsRecField GhcPs (LPat GhcPs) -> Doc
patFieldBind ctx HsFieldBind{..} =
  at ctx hfbLHS (fieldOcc ctx)
    <> includeUnless
      hfbPun
      (joinedBy "=" <> indent (hsPat ctx hfbRHS))

-- | The name of a record field.
fieldOcc :: Ctx -> FieldOcc GhcPs -> Doc
fieldOcc ctx FieldOcc{..} = name ctx foLabel

-- | An unboxed sum: the one alternative that is present, with a bar for each
-- one that is not.
unboxedSum :: ClosingIndent -> ConTag -> Arity -> Doc -> Doc
unboxedSum closing tag arity d =
  unboxedWith closing (sepBy (txt "|") (before <> [space <> d <> space] <> after))
  where
    before = replicate (tag - 1) space
    after = replicate (arity - tag) space

-- | The brackets a tuple is written with.
boxityBrackets :: Boxity -> ClosingIndent -> Doc -> Doc
boxityBrackets = \case
  Boxed -> parensWith
  Unboxed -> unboxedWith
