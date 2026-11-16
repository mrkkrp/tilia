{-# LANGUAGE DataKinds #-}

module Registry.Stack where

type Push x (xs :: [k]) = (x ': xs :: [k])
