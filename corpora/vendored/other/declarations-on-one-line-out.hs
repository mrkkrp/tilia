{-# LANGUAGE TemplateHaskell #-}

module Grid.Sizes where

sizes = [d|width = 80; height = 24|]

limits = [d|maxWidth :: Int; maxWidth = 200; minWidth = 10|]
