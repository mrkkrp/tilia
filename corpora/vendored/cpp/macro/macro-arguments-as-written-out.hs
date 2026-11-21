{-# LANGUAGE CPP #-}

-- cpp pastes the arguments of a macro into its body as they were written,
-- spaces and all, so they stay as written.
module Inspection where

import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI

#define OIDS(a,b) [TI.a,TI.b]
#define PAIR(a,b) (TI.a,TI.b)
#define BOTH(a,b) (Just a,Just b)

oids :: [Oid]
oids = OIDS (name,text)

spaced :: [Oid]
spaced = OIDS ( name ,  text )

paired :: PAIR (Oid,Oid)
paired = PAIR (name,text)

same :: (Maybe Oid, Maybe Oid) -> Bool
same BOTH (a,b) = a == b
same _ = False

long :: [Oid]
long =
  OIDS
    (characterVaryingWithAVeryLongName,timestampWithTimeZoneAndAnotherLongName)

broken :: [Oid]
broken =
  OIDS
    (name,text)
