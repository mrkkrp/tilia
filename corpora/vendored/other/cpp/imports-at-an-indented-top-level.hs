{-# LANGUAGE CPP #-}

module Terminal.Bell (bell) where
  {-
import Terminal.Sound
  -}
  import System.IO (hFlush, stdout)
#ifdef WITH_VISUAL_BELL
  import Terminal.Flash (flash)
#endif
  import Control.Monad (void)

  bell :: IO ()
  bell  =  void (putStr "\a") >> hFlush stdout
