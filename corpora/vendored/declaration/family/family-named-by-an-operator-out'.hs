{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Circuit.Path where

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
    (++) '[] ys = ys
    (++) (x ': xs) ys = x ': (xs ++ ys)
