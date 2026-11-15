module Relevance.Entries (cons) where

cons :: (Arg Nat, Term) -> Res -> Res
cons a@(Arg ai i, t) vars
    | isIrrelevant ai = applyUnless (any ((i ==) . unArg . fst) vars) (a :) vars
    | otherwise =
        a -- adding a relevant entry
          -- filter out duplicate irrelevants
            : filter (not . duplicateOf i) vars
