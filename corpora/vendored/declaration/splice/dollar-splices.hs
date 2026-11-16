{-# LANGUAGE TemplateHaskell #-}

module Shop.Schema where

import Shop.Schema.Derive

$(deriveCodec ''Customer)
$(deriveCodec ''Supplier)

$deriveEverything

$$(deriveStaged ''Shipment)

$(   deriveTable   ''Invoice   "invoices")

$( deriveTable
     ''Payment
       "payments" )

$(do codecs <- traverse deriveCodec [''Refund, ''Voucher]
     pure (concat codecs))
