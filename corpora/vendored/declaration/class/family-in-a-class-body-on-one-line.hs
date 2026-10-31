{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module M where

class C a where { type X a; f :: a -> X a }

class D a where { type Y a = r | r -> a; g :: a -> Y a }

x = [d| class E a where { type W a :: Type; h :: a } |]
