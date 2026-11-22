{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-} -- for the instance of Shed
                                      -- in Garden.Tools

{-# OPTIONS_GHC -Wno-orphans #-}

-- The empty line under a comment carrying on the remark of a pragma stays
-- between that pragma and the next, which keep none, when sorting moves it.
module Garden.Shed where
