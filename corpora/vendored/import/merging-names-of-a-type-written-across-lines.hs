module Billing.Refund where

import Billing.Status (Status (Paid), refund)
import Billing.Status
  ( Status (Refunded,
      Disputed)
  )

undo :: Status -> Status
undo _ = Refunded
