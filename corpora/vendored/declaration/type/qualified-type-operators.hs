{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grid.Size where

import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (..))
import qualified GHC.TypeLits as T

type Cells = 4 T.* 3 T.+ 2

type Border = Proxy (2 T.* (4 T.+ 3))

type Total =
  10
    T.+ 2
    T.* 3
    T.- 1

type Layers = Proxy ('[Maybe (Either Bool Char), Int] 'NE.:| '[ '[Int]])
