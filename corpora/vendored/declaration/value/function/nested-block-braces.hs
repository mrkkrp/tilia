{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StaticPointers #-}

module Journal.Replay where

replay = do { entry <- do { readEntry }; apply entry }

retry lock = do { withLock lock do { flush }; release lock }

warnUnless quiet = do { unless quiet if loud then shout else do { warn }; finish }

restore = mdo { rec { ref <- do { newRef cell }; cell <- load ref }; pure cell }

pick mode = case mode of { Eager -> run do { fetch }; Lazy -> defer }

settle = let { first = run do { fetch }; rest = defer } in first

profile = do { {-# SCC "flush" #-} do { flush }; report }

pointer = do { static do { flush }; report }
