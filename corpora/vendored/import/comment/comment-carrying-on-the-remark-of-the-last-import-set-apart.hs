-- The empty line under a comment carrying on the remark of the last import
-- stays at the end of the imports when sorting moves that import.
module Garden.Shed where

import Garden.Tools
import Garden.Seasons -- every job is planned by season
                      -- so the seasons come before anything else

data Shed = Shed
