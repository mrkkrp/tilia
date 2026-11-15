{-# LANGUAGE CPP #-}

module Terminal.Colour
  ( -- * Colours
    Colour (..),

-- A terminal without true colour knows only the sixteen named ones.
#ifdef TRUE_COLOUR
    rgb,
#else
    named,

-- The bright ones are an extension of their own.
#ifdef BRIGHT
    -- ** Bright
    bright,
#endif
#endif

    -- * Rendering
    render,
  )
where

import Terminal.Colour.Internal
