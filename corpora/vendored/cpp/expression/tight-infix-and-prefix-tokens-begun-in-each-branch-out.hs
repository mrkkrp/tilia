{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Render.Binding where

renderBinding :: Binding -> Doc
renderBinding
#if MIN_VERSION_syntax(2,9,0)
  b@( Binding
        (Info hiding relevance lock rewrite)
        (Named name)
      ) =
#else
  b@( Binding
        (Info hiding relevance lock)
        (Named name)
      ) =
#endif
    renderNamed hiding b name

strictName :: Binding -> Name
strictName
#if MIN_VERSION_syntax(2,9,0)
  !Binding
    { info = Info _ _ _ _,
      named = Named name
#else
  !Binding
    { info = Info _ _ _,
      named = Named name
#endif
    } = name

defaultInfo :: String
defaultInfo =
  show
#if MIN_VERSION_syntax(2,9,0)
    @(Maybe Info)
#else
    @(Either Text Info)
#endif
    Nothing
