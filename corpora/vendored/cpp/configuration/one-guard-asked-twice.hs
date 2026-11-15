{-# LANGUAGE CPP #-}
#if defined(HAVE_PATTERNS)
{-# LANGUAGE PatternSynonyms #-}
#endif

module Terminal.Key
  ( Key (..),
    keyCode,
#ifndef NO_NAMES
    keyName,
    keyLabel,
#if defined(HAVE_PATTERNS)
    pattern Escape,
    pattern Enter,
#endif
    keyGlyph,
#endif
  )
where

data Key = Key Int

keyCode :: Key -> Int
keyCode (Key c)  =  c

#ifndef NO_NAMES
keyName :: Key -> String
keyName (Key c)  =  "key" <> show c

keyLabel :: Key -> String
keyLabel k  =  "<" <> keyName k <> ">"

keyGlyph :: Key -> Char
keyGlyph (Key c)  =  toEnum c
#endif

#if defined(HAVE_PATTERNS)
pattern Escape :: Key
pattern Escape  =  Key 27

pattern Enter :: Key
pattern Enter  =  Key 13
#endif
