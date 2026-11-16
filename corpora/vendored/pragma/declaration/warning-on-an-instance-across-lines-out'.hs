module Vault.Token where

newtype Token = Token String

{-# WARNING expire "A token expires on its own." #-}
expire :: Token -> Token
expire = id

instance
    {-# WARNING "Showing a token prints it." #-}
    Show Token where
    show (Token t) = t

instance
    {-# WARNING "Comparing tokens takes time." #-}
    Eq Token where
    Token a == Token b = a == b

deriving instance {-# WARNING "The order says nothing." #-} Ord Token
