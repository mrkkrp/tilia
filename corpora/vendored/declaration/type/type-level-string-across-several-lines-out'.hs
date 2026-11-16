{-# LANGUAGE DataKinds #-}

module Help.Usage where

import Data.Proxy (Proxy (..))

type Usage =
    "usage: convert [--verbose] \
    \<input> <output>"

type Hint = Proxy "see --help"

type Footer =
    Proxy
        "report bugs to \
        \the issue tracker"
