{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,16,0)
{-# LANGUAGE OverloadedRecordDot #-}
#endif

module Queue.Depth where

data Queue = Queue{items :: [Int]}

depth :: Queue -> Int
depth q =
#if !MIN_VERSION_base(4,16,0)
    length (items q)
#else
#if defined(TRACE_QUEUE)
    traceShow q.items (length q.items)
#else
    length q.items
#endif
#endif
