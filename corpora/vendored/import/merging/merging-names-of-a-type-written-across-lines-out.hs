module Billing.Refund where

import Billing.Status
  ( Status
      ( Disputed,
        Paid,
        Refunded
      ),
    refund,
  )

undo :: Status -> Status
undo _ = Refunded
