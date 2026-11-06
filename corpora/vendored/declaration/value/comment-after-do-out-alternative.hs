warmEverything =
  entries
    >>= \entry -> -- one at a time, so a failure names the entry
      warm entry

reportOn entry = do -- the counters are read once, before any of this runs
  hits <- readCounter entry
  pure hits
