{-# LANGUAGE TypeFamilies #-}

module Grid.Cell where

data family Grid row col

data instance Grid Int col = IntRow [col]

data instance Grid Bool col where
  Flags :: [Bool] -> Grid Bool col
  NoFlags
    :: Grid Bool
         col

newtype instance Grid [Double] col = Readings {
  readings :: [Double]
}
