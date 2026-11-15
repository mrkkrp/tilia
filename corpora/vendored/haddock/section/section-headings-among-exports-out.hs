module Database.Query
  ( -- * Setup
    -- $setup

    -- * Language
    where_,
    on,
    groupBy,
    orderBy,

    -- ** Joins
    InnerJoin (..),
    CrossJoin (..),

    -- * Running
    select,
    delete,
    -- $running
  )
where
