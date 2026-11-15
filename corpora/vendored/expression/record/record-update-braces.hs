{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

restock c = c {crateCount = crateCount c + 10}

emptied = (restock emptyCrate) {crateCount = 0}

twice c = c {crateCount = 1} {crateSku = Sku "x"}

relabel = map (\c -> c {crateSku = Sku "y"}) . filter ((> 0) . crateCount) -- keep the full ones

clear c = c {}

deep m = m {origin.city = "Lyon", origin.postcode}

byOperator s = s {(<+>) = max}

spread c = c {
  crateCount = 3,
  crateSku = Sku "z" }
