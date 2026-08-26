{-# LANGUAGE CPP #-}

module Terminal.Legacy where

columns :: Int
columns = 80

#if !MIN_VERSION_base(4,19,0)
rows :: Int
rows = 24
#endif

title :: String
title = "session"
