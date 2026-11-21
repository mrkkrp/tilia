{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- A promoted list or tuple whose first item begins with a promoted type,
-- as an operand, under a kind signature or applied to a kind, keeps a space
-- before that item, or its opening would read as a character.
module Schema.Columns where

type Nested = '[ 'True ': '[], '[]]

type Empty = '[ '[] :: [Bool]]

type Labelled = '( 'Just "id" :: Maybe Symbol, 'False)

type Absent = '[ 'Nothing @Bool, 'Just 'True]
