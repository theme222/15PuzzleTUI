module Config where

-- Refresh rate (timer)
_REFRESH_RATE_MS :: Double
_REFRESH_RATE_MS = 4.17

_REFRESH_HELPER_MS :: Double
_REFRESH_HELPER_MS = 200

_VERSION :: String -- For some reason when I use the import Paths_15PuzzleTUI (version) thing my lsp just stops working
_VERSION = "1.1.0.0"