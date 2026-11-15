{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- A quotation ending with the dots of an abstract closed family keeps a
-- space before its closing bracket, as the two would read as one operator.
module Store.Signature where

element = [d|  type family Element c where ..   |]

plain = [d|  type family Key c  |]
