{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Generators for documents and spans.
module Tilia.Gen
  ( AnyDoc (..),
    PlainDoc (..),
    FlatSafeDoc (..),
    AnySpan (..),
    SingleLineSpan (..),
    MultiLineSpan (..),
    docTexts,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Test.QuickCheck
import Tilia.Doc.Internal
import Tilia.Span

----------------------------------------------------------------------------
-- Spans

-- | An arbitrary span.
newtype AnySpan = AnySpan Span
  deriving (Eq, Show)

instance Arbitrary AnySpan where
  arbitrary = AnySpan <$> genSpan
  shrink (AnySpan s) = AnySpan <$> shrinkSpan s

-- | A span that occupied one line.
newtype SingleLineSpan = SingleLineSpan Span
  deriving (Eq, Show)

instance Arbitrary SingleLineSpan where
  arbitrary = do
    l <- choose (1, 20)
    c0 <- choose (1, 40)
    c1 <- choose (c0, 80)
    pure (SingleLineSpan (mkSpan (l, c0) (l, c1)))

-- | A span that ran across several lines.
newtype MultiLineSpan = MultiLineSpan Span
  deriving (Eq, Show)

instance Arbitrary MultiLineSpan where
  arbitrary = do
    l0 <- choose (1, 20)
    n <- choose (1, 5)
    c0 <- choose (1, 40)
    c1 <- choose (1, 80)
    pure (MultiLineSpan (mkSpan (l0, c0) (l0 + n, c1)))

genSpan :: Gen Span
genSpan = do
  l0 <- choose (1, 20)
  n <- choose (0, 5)
  c0 <- choose (1, 40)
  c1 <- choose (1, 80)
  pure (mkSpan (l0, c0) (l0 + n, c1))

shrinkSpan :: Span -> [Span]
shrinkSpan s =
  [ mkSpan (spanStartLine s, spanStartColumn s) (spanStartLine s, spanEndColumn s)
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
            DNest <$> choose (0, 2) <*> go (n - 1),
            DAlign <$> go (n - 1),
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

shrinkDoc :: Doc -> [Doc]
shrinkDoc = \case
  DEmpty -> []
  DText t -> DText <$> filter (not . T.null) (T.inits t)
  DSpace -> [DEmpty]
  DBreak -> [DEmpty, DSpace]
  DSoftBreak -> [DEmpty]
  DHardBreak -> [DEmpty]
  DVerbatimBreak _ -> [DEmpty]
  DCloseLine -> [DEmpty]
  DHoldBack t -> DHoldBack <$> filter (not . T.null) (T.inits t)
  DCat a b -> [DEmpty, a, b] <> [DCat a' b | a' <- shrinkDoc a] <> [DCat a b' | b' <- shrinkDoc b]
  DNest n d -> [DEmpty, d] <> [DNest n d' | d' <- shrinkDoc d]
  DAlign d -> [DEmpty, d] <> [DAlign d' | d' <- shrinkDoc d]
  DGroup l d -> [DEmpty, d] <> [DGroup l d' | d' <- shrinkDoc d]
  DVariant a b -> [DEmpty, a, b]
  DLocated s d -> [DEmpty, d] <> [DLocated s d' | d' <- shrinkDoc d]
  DFence s d -> [DEmpty, d] <> [DFence s d' | d' <- shrinkDoc d]
  DCppChoice bs e -> [DEmpty, e] <> fmap snd bs
  DCppDirective _ _ -> [DEmpty]

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
  DVerbatimBreak _ -> []
  DCloseLine -> []
  DHoldBack t -> [t]
  DCat a b -> docTexts a <> docTexts b
  DNest _ d -> docTexts d
  DAlign d -> docTexts d
  DGroup _ d -> docTexts d
  DVariant a _ -> docTexts a
  DLocated _ d -> docTexts d
  DFence _ d -> docTexts d
  DCppChoice bs e -> concat [c : docTexts d | (c, d) <- bs] <> docTexts e
  DCppDirective _ t -> [t]
