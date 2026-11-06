{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Store.Lenses where

makeLenses ''Config
-- A note written against the run it stands in, and staying inside it.
makeLenses ''Options

makeLenses ''State
#if defined(PRISMS)
makePrisms ''Event
#endif
makeLenses ''Event
