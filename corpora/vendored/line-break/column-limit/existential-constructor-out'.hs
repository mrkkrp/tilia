-- A constructor with a forall that runs past column 80 stays beside the =
-- where its forall was written there, and its forall and context break
-- before its arguments do.
module Editor.Buffer where

data Buffer = forall editor content
     . (TextEditor editor, Show content, Monoid content)
    => Buffer{bufferEditor :: editor, bufferContent :: content}

data Cont u w a
    = Cont (GenHaxl u w a)
    | forall b
       . Cont u w b
            :>>=
            (b -> GenHaxl u w a -> Either SomeException (Result u w b))
    | Return (IVar u w a)
