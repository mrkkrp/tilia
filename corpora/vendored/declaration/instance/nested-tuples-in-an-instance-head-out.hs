module Survey.Point where

instance (Plot x, Plot (y, z)) => Plot (x, (y, z)) where
  plot = undefined

instance
  ( Plot x,
    Plot
      ( y,
        z
      )
  ) =>
  Plot
    ( x,
      (y, z)
    )
  where
  plot = undefined
