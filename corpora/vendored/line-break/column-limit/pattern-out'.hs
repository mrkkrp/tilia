{-# LANGUAGE Arrows #-}

-- A pattern that runs past column 80 is broken over several lines, but for
-- the patterns of a lambda, a case alternative and a proc, which keep their
-- line, since whether what follows them hangs depends on it.
module Loom.Patterns where

tension
    callSite
    ( Loom
        (Warp warpName warpCount)
        (Weft weftName weftCount)
        (Shuttle speedOfIt load)
    ) = warpCount

( Loom
    (Warp warpName warpCount)
    (Weft weftName weftCount)
    (Shuttle speedOfIt load)
    )
    `interlace` other = other

thread
    Shuttle
        { shuttleName = name
        , shuttleSpeed = speed
        , shuttleLoad = load
        , shuttleOwner = o
        } = name

spin = do
    ( warpThread
      , weftThread
      , shuttleThread
      , bobbinThread
      , spindleThread
      , heddleThread
        ) <- fetchAllThreads
    pure warpThread

weave = \(Loom (Warp warpName warpCount) (Weft weftName weftCount) (Shuttle speedOfIt)) ->
    warpCount

pick loom = case loom of
    Loom (Warp warpName warpCount) (Weft weftName weftCount) (Shuttle speedOfIt) ->
        warpCount

circuit = proc
    (Loom (Warp warpName warpCount) (Weft weftName weftCount) (Shuttle speed)) ->
    returnA -< warpCount
