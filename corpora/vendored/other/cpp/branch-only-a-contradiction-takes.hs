{-# LANGUAGE CPP #-}
module Terminal.Cell where

#if defined(WIDE_CELLS) && defined(HAVE_UNICODE)
#if !defined(HAVE_UNICODE)
width = (
#endif
columns = 80
#endif
