module Weather.Station (station) where

import Data.Map.Strict
  ( Map,

    -- Construction
    empty,
    insert,

    -- Query
    member,
  )

station :: Map String Int
station = insert "Oslo" 1 empty
