-- A comment lined up with a constructor stays under it, whether the = or
-- the bar before it begins its line, ends the line above or comes between
-- two constructors on one line. One lined up with the bars goes with what
-- follows them.
module Ledger.Account where

data Kind = Asset | -- what is owned
            Liability Int
            -- owed to others
  deriving (Eq)

data Side = Debit | Credit Int
                    -- settled at the end of the day
  deriving (Eq)

data Source
  = Salary
    -- paid monthly
  | Interest Int
    -- paid yearly
  deriving (Eq)

newtype Balance = Balance Int
                  -- in cents
  deriving (Eq)

data Term = Short
          | Long Int
          -- lined up with the bars rather than the constructors
  deriving (Eq)

-- The same under the body of an equation before its where.
total :: [Int] -> Int
total entries = foldr add 0 entries
                -- in the order posted
  where
    add = (+)
