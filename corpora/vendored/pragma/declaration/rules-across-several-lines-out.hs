module Stream.Fusion where

{-# RULES
"scan/scan"
  scan f (scan g s) =
    scan (f . g) s
"drain/fill" [2] forall n.
  drain n (fill n) =
    id
"merge/empty" forall s. merge s empty = s
"lift/const" forall x. lift (const x) = \_ -> x
  #-}

{-# RULES "batch/unbatch" forall xs. batch (unbatch xs) = xs #-}
