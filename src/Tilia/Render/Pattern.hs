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
    patArguments,
    fieldOcc,
    unboxedSum,
    boxityBrackets,
  )
where

import Data.Choice (Choice, fromBool, isTrue, pattern Is, pattern Isn't)
import Data.Foldable (fold)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, isJust)
import GHC.Hs
import GHC.Types.Basic (Arity, Boxity (..), ConTag)
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (unLoc)

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
hsPat ctx = hsPatClosing ctx 1

-- | A pattern whose outermost bracket closes where it is told to.
hsPatClosing :: Ctx -> Int -> LPat GhcPs -> Doc
hsPatClosing ctx = hsPatIn ctx (Isn't #inParens)

-- | A pattern that knows where it stands.
hsPatIn ::
  -- | The context.
  Ctx ->
  -- | Is this inside parentheses?
  Choice "inParens" ->
  -- | How many steps in from its opening bracket its outermost one closes.
  Int ->
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
  -- | How many steps in from its opening bracket its outermost one closes.
  Int ->
  -- | Where the pattern was written.
  Maybe Span ->
  -- | The pattern.
  Pat GhcPs ->
  Doc
patBody ctx inParens closing here = \case
  WildPat _ -> txt "_"
  VarPat _ n -> name ctx n
  LazyPat tok p -> keywordAt (spanOf tok) "~" <> recur p
  AsPat tok n p -> name ctx n <> keywordAt (spanOf tok) "@" <> recur p
  pat@(ParPat parens' _) ->
    inside (bracketWith closing (pairSpans parens') "(" ")") pat
  BangPat tok p -> keywordAt (spanOf tok) "!" <> recur p
  pat@(ListPat ann _) ->
    inside
      ( commaBracketWith
          (ctxCommaStyle ctx)
          closing
          (bracketSpans (al_brackets ann))
          "["
          "]"
      )
      pat
  pat@(TuplePat brackets _ boxity) ->
    inside
      ( boxityBrackets
          (ctxCommaStyle ctx)
          boxity
          closing
          (pairSpans brackets)
      )
      pat
  OrPat _ ps ->
    itemsSepBy (fromBool (isTrue inParens)) NoBrace (fmap recur (NE.toList ps))
  SumPat ann p tag arity ->
    unboxedSum closing (pairSpans (sumPatParens ann)) tag arity (ref (recur p))
  ConPat braces con details ->
    conPattern ctx inParens here (pairSpans braces) con details
  ViewPat _ e p ->
    align $
      knotExpr (ctxKnot ctx) ctx e
        >+> joinedBy (reserved ctx "->")
          <> recur p
  SplicePat _ splice -> knotSplice (ctxKnot ctx) ctx DollarSplice splice
  LitPat _ lit -> hsLit lit
  NPat tok v (isJust -> negated) _ ->
    includeWhen negated (keywordAt (spanOf tok) "-" <> negativeLiteralGap ctx)
      <> at ctx v (outputable . ol_val)
  NPlusKPat _ n k _ _ _ ->
    align $
      name ctx n
        >+> breakOrSpace
          <> txt "+"
          <> space
          <> at ctx k (outputable . ol_val)
  SigPat _ p HsPS{..} ->
    recur p >+> typeAscription ctx (spanOf p) (asSigType hsps_body)
  EmbTyPat _ (HsTP _ ty) -> txt "type" <> space <> hsType ctx ty
  InvisPat (tok, _) (HsTP _ ty) ->
    keywordAt (spanOf tok) "@" <> hsType ctx ty
  where
    recur = hsPatIn ctx inParens 1
    inside brackets =
      brackets . insideBrackets here . ref . fold . bracketed ctx inParens

-- | A constructor pattern, in whichever of its three forms.
conPattern ::
  -- | The context.
  Ctx ->
  -- | Is this inside parentheses?
  Choice "inParens" ->
  -- | Where the pattern was written.
  Maybe Span ->
  -- | Where the braces around its fields were written, if it has them.
  (Maybe Span, Maybe Span) ->
  -- | The constructor.
  LocatedN RdrName ->
  -- | What it was applied to.
  HsConPatDetails GhcPs ->
  Doc
conPattern ctx inParens here braces con details = case details of
  PrefixCon [] -> align (name ctx con)
  PrefixCon _ ->
    align (name ctx con >+> breakOrSpace <> ref (align arguments))
  RecCon _ -> name ctx con >+> breakOrNothing <> ref arguments
  InfixCon l r ->
    layoutFrom ctx (spanOf l <> spanOf r) $
      hsPatIn ctx inParens 1 l >+> ref arguments
  where
    arguments = conArguments ctx inParens here braces con details

-- | What a constructor of a pattern is applied to, as a part that what
-- follows the pattern is placed relative to, before it is lined up: its
-- arguments, its fields, or, written infix, the constructor and the
-- operand after it.
conArguments ::
  -- | The context.
  Ctx ->
  -- | Is this inside parentheses?
  Choice "inParens" ->
  -- | Where the pattern was written.
  Maybe Span ->
  -- | Where the braces around its fields were written, if it has them.
  (Maybe Span, Maybe Span) ->
  -- | The constructor.
  LocatedN RdrName ->
  -- | What it was applied to.
  HsConPatDetails GhcPs ->
  Doc
conArguments ctx inParens here braces con = \case
  PrefixCon args -> sepBy breakOrSpace (fmap (align . recur) args)
  RecCon (HsRecFields _ fields dotdot) ->
    bracketWith
      0
      braces
      "{"
      "}"
      ( insideBrackets
          here
          ( commaSep
              (ctxCommaStyle ctx)
              (fmap field (visibleFields dotdot fields))
          )
      )
  InfixCon l r ->
    attachOperator
      ctx
      Normal
      id
      (spanOf l)
      (spanOf con)
      (spanOf r)
      (name ctx con)
      (recur r)
  where
    recur = hsPatIn ctx inParens 1
    field = either wildcard (at_ ctx (patFieldBind ctx))
    wildcard l = at ctx l (const (txt ".."))
    visibleFields dotdot fields = case dotdot of
      Nothing -> Right <$> fields
      Just l -> (Right <$> fields) <> [Left l]

-- | The part of a pattern that what follows it is placed relative to: what
-- its constructor is applied to, marks and a name bound with \@ aside, and
-- otherwise the whole.
patArguments ::
  -- | The context.
  Ctx ->
  -- | The pattern.
  LPat GhcPs ->
  Doc
patArguments ctx p = fromMaybe (hsPat ctx p) (arguments p)
  where
    arguments q = case unLoc q of
      ConPat _ _ (PrefixCon []) -> Nothing
      ConPat braces con details ->
        Just
          ( conArguments
              ctx
              (Isn't #inParens)
              (spanOf q)
              (pairSpans braces)
              con
              details
          )
      AsPat _ _ q' -> arguments q'
      BangPat _ q' -> arguments q'
      LazyPat _ q' -> arguments q'
      _ -> Nothing

-- | What a pattern in brackets holds between them, if it is one.
bracketed ::
  -- | The context.
  Ctx ->
  -- | Is this inside parentheses?
  Choice "inParens" ->
  -- | The pattern.
  Pat GhcPs ->
  Maybe Doc
bracketed ctx inParens = \case
  ParPat _ p -> Just (hsPatIn ctx (Is #inParens) 1 p)
  ListPat _ ps -> Just (separated ps)
  TuplePat _ ps _ -> Just (separated ps)
  SumPat _ p _ _ -> Just (hsPatIn ctx inParens 1 p)
  _ -> Nothing
  where
    separated =
      commaSep (ctxCommaStyle ctx)
        . fmap (align . hsPatIn ctx inParens 1)

-- | One field of a record pattern.
patFieldBind :: Ctx -> HsRecField GhcPs (LPat GhcPs) -> Doc
patFieldBind ctx HsFieldBind{..}
  | hfbPun = at ctx hfbLHS (fieldOcc ctx)
  | otherwise =
      at ctx hfbLHS (fieldOcc ctx) >+> joinedBy "=" <> hsPat ctx hfbRHS

-- | The name of a record field.
fieldOcc :: Ctx -> FieldOcc GhcPs -> Doc
fieldOcc ctx FieldOcc{..} = name ctx foLabel

-- | An unboxed sum: the one alternative that is present, with a bar for each
-- one that is not.
unboxedSum ::
  -- | How many steps in from the opening bracket the closing one goes.
  Int ->
  -- | Where the brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | Which alternative is present.
  ConTag ->
  -- | How many alternatives there are.
  Arity ->
  -- | The alternative.
  Doc ->
  Doc
unboxedSum closing brackets tag arity d =
  spacedBracket
    0
    closing
    brackets
    "(#"
    "#)"
    (sepBy (txt "|") (before <> [space <> d <> space] <> after))
  where
    before = replicate (tag - 1) space
    after = replicate (arity - tag) space

-- | The brackets a tuple is written with.
boxityBrackets ::
  -- | Where the commas go.
  CommaStyle ->
  -- | Whether the tuple is boxed.
  Boxity ->
  -- | How many steps in from the opening bracket the closing one goes.
  Int ->
  -- | Where the brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | The items, separated.
  Doc ->
  Doc
boxityBrackets commas boxity closing brackets = case boxity of
  Boxed -> commaBracketWith commas closing brackets "(" ")"
  Unboxed -> spacedBracket (commaWidth commas) closing brackets "(#" "#)"
