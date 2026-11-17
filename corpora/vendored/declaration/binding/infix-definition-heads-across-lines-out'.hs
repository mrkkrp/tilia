module Audio.Mix where

(<+>) :: Track -> Track -> Gain -> Track
( a
    <+> b
    )
    g = amplify g (overlay a b)

mixdown :: Track -> Track -> Gain -> Track
mixdown x y = go
    where
        ( a
            `blend` b
            )
            h = crossfade h a b
        go = (x `blend` y)
