{-# LANGUAGE MultilineStrings #-}

module Canvas.Script where

import Foreign.C.Types (CInt (..))

foreign import javascript
  """
  ((width, height) => width * height)
  """
  canvasArea :: Int -> Int -> Int

foreign import ccall
  "static vendor/canvas/include/canvas_surface.h \
  \canvas_clear"
  canvasClear :: CInt -> IO ()
