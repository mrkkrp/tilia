{-# LANGUAGE StaticPointers #-}

module Cluster.Task where

import GHC.StaticPtr (StaticPtr)

retries :: StaticPtr Int
retries = static 3

workers :: StaticPtr [String]
workers =
  static
    [ "alpha",
      "beta",
      "gamma"
    ]

job :: StaticPtr (IO ())
job =
  static
    ( runJob
        "nightly"
    )
