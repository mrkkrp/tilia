{-# LANGUAGE CPP #-}
#if defined(HAVE_PATTERNS)
{-# LANGUAGE PatternSynonyms #-}
#endif

module Terminal.Key
  ( Key (..),
    keyCode,
#if defined(HAVE_PATTERNS)
#ifndef NO_NAMES
    keyName,
    keyLabel,
    pattern Escape,
    pattern Enter,
    keyGlyph,
#endif
#else
#ifndef NO_NAMES
    keyName,
    keyLabel,
    keyGlyph,
#endif
#endif
  )
where

data Key = Key Int

keyCode :: Key -> Int
keyCode (Key c) = c

#ifndef NO_NAMES
keyName :: Key -> String
keyName (Key c) = "key" <> show c

keyLabel :: Key -> String
keyLabel k = "<" <> keyName k <> ">"

keyGlyph :: Key -> Char
keyGlyph (Key c) = toEnum c
#endif

#if defined(HAVE_PATTERNS)
pattern Escape :: Key
pattern Escape = Key 27

pattern Enter :: Key
pattern Enter = Key 13
#endif
