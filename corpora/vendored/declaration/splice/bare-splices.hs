{-# LANGUAGE TemplateHaskell #-}

module Shop.Catalogue where

import Shop.Schema.Derive (deriveCodec, deriveTable)

deriveCodec ''Product

deriveTable
  ''Category
    "categories"

[d| currency :: String
    currency = "EUR" |]

[d| type Quantity = * |]

[d|
  data Unit = Piece | Kilogram | Litre
    deriving (Eq, Show)
  |]

do
  codecs <- traverse deriveCodec [''Discount, ''Bundle]
  pure (concat codecs)

do [d| catalogueVersion = 3 |]
