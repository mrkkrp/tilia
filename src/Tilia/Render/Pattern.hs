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
    fieldOcc,
    unboxedSum,
  )
where

import Data.Choice (Choice, fromBool, isTrue, pattern Is, pattern Isn't)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.Basic (Arity, Boxity (..), ConTag)
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (GenLocated (..))
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Layout
import Tilia.Render.Name
import Tilia.Render.Type
import Tilia.Span
import Tilia.Span.Ghc

-- | A pattern.
hsPat :: Ctx -> LPat GhcPs -> Doc
hsPat ctx = hsPatIn ctx NoBrace (Isn't #inAsPat)

-- | A pattern that knows where it stands.
hsPatIn ::
  -- | The context.
  Ctx ->
  -- | Bracing an or-pattern inherits.
  Bracing ->
  -- | Is this inside an as-pattern?
  Choice "inAsPat" ->
  -- | The pattern.
  LPat GhcPs ->
  Doc
hsPatIn ctx bracing inAsPat l = at ctx l (patBody ctx bracing inAsPat (spanOf l))

-- | Every shape a pattern can take.
patBody ::
  -- | The context.
  Ctx ->
  -- | Bracing an or-pattern inherits.
  Bracing ->
  -- | Is this inside an as-pattern?
  Choice "inAsPat" ->
  -- | Where the pattern was written.
  Maybe Span ->
  -- | The pattern.
  Pat GhcPs ->
  Doc
patBody ctx bracing inAsPat here = \case
  WildPat _ -> txt "_"
  VarPat _ n -> name ctx n
  LazyPat _ p -> txt "~" <> recur p
  AsPat _ n p -> name ctx n <> txt "@" <> hsPatIn ctx bracing (Is #inAsPat) p
  ParPat _ p -> parensWith Indented (insideBrackets here (recur p))
  BangPat _ p -> txt "!" <> recur p
  ListPat _ ps -> bracketsWith Indented (insideBrackets here (commaSep (fmap recur ps)))
  TuplePat _ ps boxity ->
    tupleBrackets boxity (insideBrackets here (commaSep (fmap (align . recur) ps)))
  OrPat _ ps ->
    itemsSepBy (fromBool (isTrue inAsPat)) bracing (fmap recur (NE.toList ps))
  SumPat _ p tag arity -> unboxedSum Indented tag arity (recur p)
  ConPat _ con details -> conPattern ctx bracing inAsPat here con details
  ViewPat _ e p ->
    align $
      knotExpr (ctxKnot ctx) ctx plainSite e
        <> joinedBy "->"
        <> indent (recur p)
  SplicePat _ splice -> knotSplice (ctxKnot ctx) ctx DollarSplice splice
  LitPat _ lit -> outputable lit
  NPat _ v (isJust -> negated) _ ->
    includeWhen negated (txt "-" <> negativeGap ctx)
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
    recur = hsPatIn ctx bracing inAsPat

-- | A constructor pattern, in whichever of its three forms.
conPattern ::
  -- | The context.
  Ctx ->
  -- | Bracing an or-pattern inherits.
  Bracing ->
  -- | Is this inside an as-pattern?
  Choice "inAsPat" ->
  -- | Where the pattern was written.
  Maybe Span ->
  -- | The constructor.
  LocatedN RdrName ->
  -- | What it was applied to.
  HsConPatDetails GhcPs ->
  Doc
conPattern ctx bracing inAsPat here con = \case
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
        <> breakOrSpace
        <> indent (name ctx con <> space <> recur r)
  where
    recur = hsPatIn ctx bracing inAsPat
    field = either wildcard (at_ ctx (patFieldBind ctx))
    wildcard l = at ctx l (const (txt ".."))
    visibleFields dotdot fields = case dotdot of
      Nothing -> Right <$> fields
      Just l@(L _ (RecFieldsDotDot n)) ->
        (Right <$> take n fields) <> [Left l]

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

-- | The brackets a tuple pattern is written with.
tupleBrackets :: Boxity -> Doc -> Doc
tupleBrackets = \case
  Boxed -> parensWith Indented
  Unboxed -> unboxedWith Indented

-- | With @NegativeLiterals@ on, @- 1@ and @-1@ are different expressions, so
-- the minus of a negated literal has to keep its distance.
negativeGap :: Ctx -> Doc
negativeGap ctx = includeWhen (extensionOn ctx NegativeLiterals) space
