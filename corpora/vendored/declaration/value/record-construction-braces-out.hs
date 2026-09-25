{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

emptyCrate = Crate{crateSku = Sku "none", crateCount = 0}

unknownCrate = Crate{}

crateOf crateSku crateCount = Crate{crateSku, crateCount}

crateHere = let crateSku = Sku "here"; crateCount = 1 in Crate{..}

weighted = Scale{(<+>) = (+), unit = 0}

qualified = Inventory.Crate{Inventory.crateCount = 2, ..}
  where
    crateSku = Sku "q"

nested = Pallet{palletCrates = [Crate{crateSku = Sku "a", crateCount = 1}], palletWrapped = True}

argument = ship Letter{letterWeight = 20} (Sku "b")

tall =
  Box
    { boxWeight = 12,
      boxFragile = False
    }
