module Audio.Mix where

infixl 6 <+>

(<+>) :: Track -> Track -> Gain -> Track
(a <+> b) g = amplify g (overlay a b)

fadeInto :: Track -> Track -> Seconds -> Track
(a `fadeInto` b) t = crossfade t a b

duckUnder :: Track -> Track -> Gain -> Track
(voice `duckUnder` music) g
  | silent voice = music
  | otherwise = overlay voice (amplify g music)
