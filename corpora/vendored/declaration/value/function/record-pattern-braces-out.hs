{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

weight Letter{letterWeight} = letterWeight
weight Box{boxWeight = w, ..} = if boxFragile then w + 1 else w
weight Pallet{} = 0

isLetter = \case Letter{} -> True; _ -> False

heavier = \cases Box{boxWeight = a} Box{boxWeight = b} -> a > b; _ _ -> False

describe p@Pallet{palletCrates = Crate{crateCount} : _} = (p, crateCount)

counted crate = case crate of
  Crate{crateCount = 0} -> "empty"
  Crate{..} -> show crateCount

unwrap
  Pallet
    { palletCrates,
      palletWrapped = True
    } = palletCrates
