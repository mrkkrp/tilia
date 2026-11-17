{-# LANGUAGE TypeAbstractions #-}

-- A lambda keeps the space before an invisible pattern, without which the
-- backslash and the @ would read as one operator.
module Shape.Measure where

measure :: forall s. Shape s => s -> Double
measure @s shape = area @s shape / perimeter @s shape

withShape :: (forall s. Shape s => s -> r) -> Blob -> r
withShape k blob = k (decodeShape blob)

blobArea :: Blob -> Double
blobArea = withShape (\ @s shape -> area @s shape)
