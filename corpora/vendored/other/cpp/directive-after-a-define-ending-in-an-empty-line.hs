{-# LANGUAGE CPP #-}

module Terminal.Limits where

#define LIMIT \
  80 \

#include "terminal.h"

width :: Int
width  =  LIMIT
