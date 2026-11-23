{-# LANGUAGE CPP #-}

-- A comment and a #define right above a conditional whose branch begins
-- with imports stay where they were written, once, rather than going into
-- the branch and a copy of them into an #else.
module Memory.Pages where

import Data.Word (Word64)

-- Sizes in bytes.

-- The page size the allocator assumes.
#define PAGE_SIZE 4096

#if defined(LINUX)
import System.Posix.Types (CSize)
#endif

pageSize :: Word64
pageSize = PAGE_SIZE
