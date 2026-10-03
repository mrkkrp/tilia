module Billing.Payment where

import Billing.Method (Method, Card (Visa))
import Billing.Method (Method (Cash), Card (..))
import Billing.Status (Status (Paid))
import Billing.Status (Status (Due, Paid), settle, settle)
import Billing.Status (settle)

pay :: Method -> Status
pay _ = Paid
