{-# LANGUAGE CPP #-}

module Shape.Render where

-- An argument only some configurations have.
render
  :: Shape
#if defined(WITH_SCALE)
  -> Double
#endif
  -> IO ()
render = undefined

-- An argument every configuration has its own of, with a comment above it.
outline
  :: Shape
#if defined(WITH_DOUBLE)
  -- In doubles.
  -> Double
#else
  -- In floats.
  -> Float
#endif
  -> IO ()
outline = undefined

-- A context only some configurations have, before a type over several
-- lines and before one on a line of its own.
trace ::
#if defined(WITH_CALLSTACK)
  (HasCallStack) =>
#endif
  String
  -> [(String, Int)]
  -> IO ()
trace = undefined

traceShape ::
#if defined(WITH_CALLSTACK)
  (HasCallStack) =>
#endif
  Shape -> IO ()
traceShape = undefined

-- A forall every configuration has its own of.
measure
#if defined(WITH_UNITS)
  :: forall unit
   . unit
#else
  :: forall a
   . a
#endif
  -> Double
measure = undefined

data Size = Size
  { width
#if defined(WITH_DOUBLE)
      :: Double
#else
      :: Float
#endif
  }

-- Arguments only some configurations have, each with a Haddock.
data Shape where
  Square
    :: Int
#if defined(WITH_SIDE)
    -- ^ The side.
    -> Int
    -- ^ The other side.
#endif
    -> Shape
