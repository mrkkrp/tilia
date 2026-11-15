{-# LANGUAGE TemplateHaskell #-}

module Order.Derive where

priceField :: Name
priceField = 'price

orderType :: Name
orderType = ''Order

ledgerType :: Q Type
ledgerType = [t|Map String Order|]

codePattern :: Q Pat
codePattern = [p|(code, _)|]
