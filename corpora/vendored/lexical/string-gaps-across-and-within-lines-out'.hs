module Help.Usage where

usage :: String
usage =
    "Usage: brew [options] recipe\n\
    \  --dry-run   show what would be done\n\
    \  --verbose   say more\n"

banner :: String
banner = "brew \ \version 2"
