{-# LANGUAGE CPP #-}

module Terminal.Win32 where

{-
typedef struct _KEY_EVENT_RECORD {
    BOOL bKeyDown;
    WORD wRepeatCount;
}
#ifdef __GNUC__
/* gcc's alignment is not what win32 expects */
 PACKED
#endif
KEY_EVENT_RECORD;
-}
data KeyEvent = KeyEvent
  { keyDown   :: Bool
  , keyRepeat :: Int
  }
