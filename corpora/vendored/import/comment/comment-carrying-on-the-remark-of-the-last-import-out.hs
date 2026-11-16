module Garden.Shed where

import Garden.Seasons -- every job is planned by season
                      -- so the seasons come before anything else
import Garden.Tools

data Shed = Shed
