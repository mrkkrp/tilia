module Kiln.Config where

quiet config =
  config{ -- keep the rest
          -- as it was
    verbose = False }

defaults = Config { -- the factory settings
  verbose = True }

isVerbose Config { -- only this
  verbose = v } = v
