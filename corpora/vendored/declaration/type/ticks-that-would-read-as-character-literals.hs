{-# LANGUAGE DataKinds #-}

module Music.Pitch where

import Data.Proxy (Proxy (..))

data Pitch = C | C' | D | D'

type Sharp = Proxy ' C'

type Chord = Proxy '[ 'Just ' D', 'Nothing]

type Voices = Proxy '[Maybe Pitch,   Either Pitch Int]
