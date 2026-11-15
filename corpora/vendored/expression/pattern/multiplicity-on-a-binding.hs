{-# LANGUAGE LinearTypes #-}

module Handle.Linear where

transfer :: Handle %1 -> Handle
transfer h = let %1 moved = h in moved

borrow :: Handle -> (Handle, Handle)
borrow h = (kept, copy)
  where
    %Many kept = h
    %Many   copy = h
