{-# LANGUAGE CPP #-}

module Queue.Chan where

#define STRICT(x) {-# UNPACK #-} !(x)

data Chan a = Chan STRICT(Int)
                   STRICT([a])
  deriving (Eq)
