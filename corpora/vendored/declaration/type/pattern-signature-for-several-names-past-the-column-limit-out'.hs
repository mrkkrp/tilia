{-# LANGUAGE PatternSynonyms #-}

module Weather.Code where

newtype Code = Code Int

pattern
    Clear
    , Cloudy
    , Overcast
    , Drizzle
    , Rain
    , Thunderstorm
    , Snow
    , Hail
    , Fog :: Code
pattern Clear = Code 0
pattern Cloudy = Code 1
pattern Overcast = Code 2
pattern Drizzle = Code 3
pattern Rain = Code 4
pattern Thunderstorm = Code 5
pattern Snow = Code 6
pattern Hail = Code 7
pattern Fog = Code 8
