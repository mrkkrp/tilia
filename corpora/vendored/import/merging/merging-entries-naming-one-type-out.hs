module Billing.Payment where

import Billing.Method (Card (..), Method (Cash))
import Billing.Status (Status (Due, Paid), settle)

pay :: Method -> Status
pay _ = Paid
