{-# LANGUAGE GADTs #-}
{-# LANGUAGE LinearTypes #-}

newtype Sku = Sku{unSku :: Text}

data Crate = Crate{crateSku :: Sku, crateCount :: Int}

data Parcel
  = Letter{letterWeight :: Int}
  | Box{boxWeight :: !Int, boxFragile :: {-# UNPACK #-} !Bool}
  | Pallet
      { palletCrates :: [Crate],
        palletWrapped :: Bool
      }

data Lease a = Lease{leased %1 :: a, leaseDays :: Int}

data Shipment where
  Shipment :: {shipmentParcels :: [Parcel]} -> Shipment

data Manifest = Manifest
  { manifestId :: Int,
    manifestShipments :: [Shipment]
  }
