{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generators for documents and spans.
module Tilia.Gen
  ( AnyDoc (..),
    PlainDoc (..),
    FlatSafeDoc (..),
    AnySpan (..),
    docTexts,
  )
where

import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck

import Tilia.Doc
import Tilia.Span

----------------------------------------------------------------------------
-- Spans

-- | An arbitrary span.
newtype AnySpan = AnySpan Span
  deriving (Eq, Show)

instance Arbitrary AnySpan where
  arbitrary = AnySpan <$> genSpan
  shrink (AnySpan s) = AnySpan <$> shrinkSpan s

genSpan :: Gen Span
genSpan = do
  l0 <- choose (1, 20)
  n <- choose (0, 5)
  c0 <- choose (1, 40)
  c1 <- choose (1, 80)
  pure (mkSpan (l0, c0) (l0 + n, c1))

shrinkSpan :: Span -> [Span]
shrinkSpan s =
  [ mkSpan
      (spanStartLine s, spanStartColumn s)
      (spanStartLine s, spanEndColumn s)
  | spanStartLine s /= spanEndLine s
  ]

----------------------------------------------------------------------------
-- Documents

-- | Text for a 'DText' node.
genText :: Gen Text
genText = T.pack <$> resize 4 (listOf1 (elements "abcxyz(),;"))

-- | Any document at all.
newtype AnyDoc = AnyDoc Doc
  deriving (Eq, Show)

instance Arbitrary AnyDoc where
  arbitrary = AnyDoc <$> sized (genDoc True True)
  shrink (AnyDoc d) = AnyDoc <$> shrinkDoc d

-- | A document with no 'DVariant'.
newtype PlainDoc = PlainDoc Doc
  deriving (Eq, Show)

instance Arbitrary PlainDoc where
  arbitrary = PlainDoc <$> sized (genDoc False True)
  shrink (PlainDoc d) = PlainDoc <$> shrinkDoc d

-- | A document that cannot break on its own.
newtype FlatSafeDoc = FlatSafeDoc Doc
  deriving (Eq, Show)

instance Arbitrary FlatSafeDoc where
  arbitrary = FlatSafeDoc <$> sized (genDoc False False)
  shrink (FlatSafeDoc d) = FlatSafeDoc <$> shrinkDoc d

-- | Build a document.
genDoc ::
  -- | Whether 'DVariant' may appear.
  Bool ->
  -- | Whether things that force a break may appear.
  Bool ->
  -- | The size parameter.
  Int ->
  Gen Doc
genDoc withVariant withBreaks = go
  where
    go n
      | n <= 1 = leaf
      | otherwise = oneof (leaf : branches)
      where
        half = n `div` 2
        branches =
          [ DCat <$> go half <*> go half,
            DAligned 0 <$> go (n - 1),
            ( \s steps a b ->
                DIndented steps (DReferent s a) b
            )
              <$> liftArbitrary genSpan
              <*> choose (0, 2)
              <*> go half `suchThat` (/= DEmpty)
              <*> go half,
            DCppMarginNote <$> go (n - 1),
            DLocated <$> genSpan <*> go (n - 1),
            DFence <$> genSpan <*> go (n - 1)
          ]
            <> [ DGroup <$> elements [Flat, Broken] <*> go (n - 1)
               | withBreaks
               ]
            <> [ DVariant <$> go half <*> go half
               | withVariant
               ]
    leaf =
      oneof $
        [ pure DEmpty,
          DText <$> genText,
          pure DSpace,
          pure DBreak,
          pure DSoftBreak
        ]
          <> (if withBreaks then [pure DHardBreak] else [])

-- | Smaller documents, none with a relation measuring from a pointer.
shrinkDoc ::
  -- | The document.
  Doc ->
  [Doc]
shrinkDoc = \case
  DEmpty -> []
  DText t -> DText <$> filter (not . T.null) (T.inits t)
  DSpace -> [DEmpty]
  DBreak -> [DEmpty, DSpace]
  DSoftBreak -> [DEmpty]
  DHardBreak -> [DEmpty]
  DVerbatimBreak _ _ -> [DEmpty]
  DCloseLine -> [DEmpty]
  DCloseLineUnlessAfterOpener _ -> [DEmpty, DCloseLine]
  DHoldBack s t -> DHoldBack s <$> filter (not . T.null) (T.inits t)
  DCat a b ->
    [DEmpty, a, b]
      <> [DCat a' b | a' <- shrinkDoc a]
      <> [DCat a b' | b' <- shrinkDoc b]
  DReferent k d -> [DEmpty, d] <> [DReferent k d' | d' <- shrinkDoc d]
  DIndented n a d ->
    [DEmpty, a, d]
      <> [DIndented n a' d | a' <- shrinkDoc a, not (pointer a')]
      <> [DIndented n a d' | d' <- shrinkDoc d]
  DAligned n d -> [DEmpty, d] <> [DAligned n d' | d' <- shrinkDoc d]
  DCppMarginNote d -> [DEmpty, d] <> [DCppMarginNote d' | d' <- shrinkDoc d]
  DGroup l d -> [DEmpty, d] <> [DGroup l d' | d' <- shrinkDoc d]
  DVariant a b -> [DEmpty, a, b]
  DLocated s d -> [DEmpty, d] <> [DLocated s d' | d' <- shrinkDoc d]
  DFence s d -> [DEmpty, d] <> [DFence s d' | d' <- shrinkDoc d]
  DSeparator e l t ->
    [DEmpty, l, t]
      <> [DSeparator e l' t | l' <- shrinkDoc l]
      <> [DSeparator e l t' | t' <- shrinkDoc t]
  DCppChoice _ bs e -> [DEmpty, e] <> fmap snd bs
  DCppDirective _ _ -> [DEmpty]
  DDeclarationsStart -> [DEmpty]
  where
    pointer a = namesOnly a && isJust (referentIn a)

-- | Every fragment of literal text the document contains, in order.
--
-- Undefined in the presence of 'DVariant', which contributes one of two
-- possible sequences depending on a layout this function cannot see; that
-- is what 'PlainDoc' exists to exclude.
docTexts :: Doc -> [Text]
docTexts = \case
  DEmpty -> []
  DText t -> [t]
  DSpace -> []
  DBreak -> []
  DSoftBreak -> []
  DHardBreak -> []
  DVerbatimBreak _ _ -> []
  DCloseLine -> []
  DCloseLineUnlessAfterOpener _ -> []
  DHoldBack _ t -> [t]
  DCat a b -> docTexts a <> docTexts b
  DReferent _ d -> docTexts d
  DIndented _ a d -> docTexts a <> docTexts d
  DAligned _ d -> docTexts d
  DCppMarginNote d -> docTexts d
  DGroup _ d -> docTexts d
  DVariant a _ -> docTexts a
  DLocated _ d -> docTexts d
  DFence _ d -> docTexts d
  DSeparator LeadingSeparator l _ -> docTexts l
  DSeparator TrailingSeparator _ t -> docTexts t
  DCppChoice _ bs e -> concat [c : docTexts d | (c, d) <- bs] <> docTexts e
  DCppDirective _ t -> [t]
  DDeclarationsStart -> []
