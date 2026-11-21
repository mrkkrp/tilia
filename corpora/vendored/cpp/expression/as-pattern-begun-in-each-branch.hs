{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Render.Binding where

renderBinding :: Binding -> Doc
renderBinding
#if MIN_VERSION_syntax(2,9,0)
  b@(Binding (Info hiding relevance lock rewrite)
#else
  b@(Binding (Info hiding relevance lock)
#endif
     (Named name)) = renderNamed hiding b name

strictName :: Binding -> Name
strictName
#if MIN_VERSION_syntax(2,9,0)
  !Binding{info = Info _ _ _ _
#else
  !Binding{info = Info _ _ _
#endif
          , named = Named name} = name

bindingName :: Binding -> Text
bindingName binding =
#if MIN_VERSION_syntax(2,9,0)
  (named binding
#else
  (legacyNamed binding
#endif
     ).text
