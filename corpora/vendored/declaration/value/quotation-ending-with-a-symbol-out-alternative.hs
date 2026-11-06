{-# LANGUAGE TemplateHaskell #-}

-- A quotation whose body may end with a symbol character keeps a space
-- inside each bracket, as the closing one would read as part of an operator.
module Chain.Quoted where

fixities = [d| infixl 6 +++ |]

method = [d| class Joinable a where infixr 5 <+> |]

local = [d| joined = x +++ y where infixl 6 +++ |]

kinded = [| Proxy :: Proxy * |]

typedKinded = [|| Proxy :: Proxy * ||]

plain = [d|joined = x ++ y|]
