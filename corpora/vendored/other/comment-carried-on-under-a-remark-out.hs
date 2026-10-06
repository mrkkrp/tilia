-- A comment lined up under the remark that ends the line above carries that
-- remark on, and stays under it.
data Directive
  = Wrapper String -- use this wrapper
  | ActionType String -- type signature of actions,
                      -- with optional typeclasses
  | TypeClass String

data Record = Record
  { actionType :: String, -- type signature of actions,
                          -- with optional typeclasses
    typeClass :: String
  }

carriedOn = a + b -- said once
                  -- and never twice

nextOne = 1
