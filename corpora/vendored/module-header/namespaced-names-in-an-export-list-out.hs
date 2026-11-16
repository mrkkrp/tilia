{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}

module Garden.Plot
  ( type (+:) (..),
    type (:-) (Edged),
    pattern Fallow,
    Plot,
  )
where

data a +: b = Beside a b

data a :- b = Edged a b

data Plot = Sown | Bare

pattern Fallow :: Plot
pattern Fallow = Bare
