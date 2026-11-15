{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

pattern Pair {left, right} = (left, right)

pattern Dozen {dozenSku} <- Crate dozenSku 12
  where
    Dozen s = Crate s 12

pattern Named {named} <- (lookupName -> Just named)

pattern Spanning {
  spanFrom,
  spanTo } = Range spanFrom spanTo
