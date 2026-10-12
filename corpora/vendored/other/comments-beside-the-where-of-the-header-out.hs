-- Comments written before and after the where of the module header stay on
-- their side of it.
module Ledger.Book
-- everything is exported
where {- until the API settles -}

import Ledger.Entry (Entry)

opening :: Entry -> Int
opening _ = 0
