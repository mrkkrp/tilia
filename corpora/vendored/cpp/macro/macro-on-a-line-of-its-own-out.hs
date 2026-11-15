{-# LANGUAGE CPP #-}

module Survey.Result where

#ifdef WITH_TRACE
#define TRACE(a) , trace a
#else
#define TRACE(a)
#endif

data Result = Result
  { passed :: Bool,
    message :: String
    TRACE(:: [String])
  }

done :: Result
done =
  Result
    { passed = True,
      message = ""
      TRACE(= [])
    }
