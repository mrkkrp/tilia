{-# LANGUAGE Arrows #-}

module Circuit.Switch where

switch threshold = proc level ->
  if level > threshold then alarm -< level
  else returnA -< level

smooth = proc sample -> let halved = sample `div` 2 in average -< halved

relay route = proc packet -> do
  let hops = length (path packet)
  if hops > 8 then discard -< packet else forward route -< packet

limit ceiling = proc reading ->
  let clipped = min ceiling reading
      scaled = clipped * 2
   in let shown = show scaled in display -< shown
