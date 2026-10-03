module Shapes.Area where

import Data.Either
  ( Either (Left, Right),
    either,
  )
import Shapes.Kind
  ( Shape (Circle, Square),
    Unit (..),
    area,
  )

describe :: Shape -> Either String Double
describe s = Right (area s)
