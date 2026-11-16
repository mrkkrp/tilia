{-# LANGUAGE QuasiQuotes #-}

module Storage.Schema (migrations) where

migrations :: [Query]
migrations =
  [ [sql|CREATE TABLE shelf (id serial PRIMARY KEY)|], -- the first release
    -- labels came with 1.2
    [sql|ALTER TABLE shelf ADD label text|],
    [sql|ALTER TABLE shelf DROP label|] -- labels have their own table now
  ]
