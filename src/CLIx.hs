module CLIx where

import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Data.List.Split
import Ipair
import Config (_VERSION)
-- import Data.Version (showVersion)
-- import Paths_15PuzzleTUI (version)

helpText :: String
helpText = unlines [
        "15PuzzleTUI: A TUI game based on the 15 slider puzzle",
        "",
        "./15p [OPTIONS]",
        "",
        "-h, --help           Show this help menu",
        "-v, --version        Show the current version",
        "-s, --size INT       Specify the starting grid size",
        "           INTxINT   Examples: 4 5x2 3x3"
    ]

versionText :: String
versionText = "15PuzzleTUI version " ++ _VERSION 


parseSize :: String -> Ipair
parseSize str =
    let splitStr = splitOn "x" str
        err = error "Invalid grid size"
        errIfInvalid (x, y) = if x < 2 || y < 2 then err else (x, y)
    in  case length splitStr of
            1 -> let val = fromMaybe err (readMaybe str)
                 in errIfInvalid (val, val)
            2 -> let val1 = fromMaybe err $ readMaybe $ head splitStr
                     val2 = fromMaybe err $ readMaybe $ splitStr !! 1
                 in  errIfInvalid (val1, val2)
            _ -> err


