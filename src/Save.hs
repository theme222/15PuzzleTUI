{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Save where

import System.Directory (getXdgDirectory, XdgDirectory(XdgData), createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Ipair
import Text.Read
import Data.Maybe (isJust, mapMaybe)
import Data.List (sort)
import ColorScheme
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, encode, decode, decodeFileStrict, Value (..), (.=), encodeFile)
import qualified Data.Aeson.Types as AT 
import Data.Aeson.Key

data Leaderboard = Leaderboard {
    leaderboardSize :: Ipair,
    leaderboardRankings :: [Int]
}

formatLeaderboardRankings :: [Int] -> [Int]
formatLeaderboardRankings rankings = take 5 $ sort rankings

-- Gets the leaderboard json file path
leaderboardPath :: Ipair -> IO FilePath
leaderboardPath size = do 
    dir <- getXdgDirectory XdgData "15p"
    createDirectoryIfMissing True (dir </> "leaderboards")
    pure (dir </> "leaderboards" </> show size ++ ".txt")
    
-- Loads the leaderboard from the json file
loadLeaderboard :: Ipair -> IO Leaderboard
loadLeaderboard size = do
    path <- leaderboardPath size
    exists <- doesFileExist path
    if not exists then pure $ Leaderboard size []
    else do  
        content <- readFile path
        pure $ Leaderboard size $ formatLeaderboardRankings $ mapMaybe readMaybe $ splitOn "\n" content  
        
-- Lets see how long this bind operator lasts in my source code before I think its too unreadable and I change it back to a do
-- 15:06 17 may 2026
-- 16:03 18 may 2026 i gotta remove it lol the file path became the content and the content became the filepath :skull:
storeLeaderboard :: Leaderboard -> IO ()
storeLeaderboard leaderboard = do
    path <- leaderboardPath (leaderboardSize leaderboard)
    writeFile path (unlines $ map show (leaderboardRankings leaderboard))

data TileType = Fill | Border | Invisible deriving (Eq)

instance Show TileType where
    show Fill = "fill"
    show Border = "border"
    show Invisible = "invisible"

data Settings = Settings {
    settingsTileType :: TileType,
    settingsColorScheme :: ColorScheme, -- Pass through both the value and the position (just in case tho idk)
    settingsGridSize :: Ipair, -- Rows, columns
    settingsRowHover :: Int,
    settingsRefreshRate :: Int 
} deriving (Generic)

instance ToJSON TileType where
    toJSON Fill = AT.String "fill"
    toJSON Border = AT.String "border"
    toJSON Invisible = AT.String "invisible"
instance FromJSON TileType where
    parseJSON (AT.String "fill") = pure Fill
    parseJSON (AT.String "border") = pure Border
    parseJSON (AT.String "invisible") = pure Invisible
    parseJSON _ = fail "Invalid tile type"

instance ToJSON ColorScheme where
    toJSON (ColorScheme name _) = AT.toJSON name

instance FromJSON ColorScheme where
    parseJSON (AT.String name) = case name of
        "fringe" -> pure ColorScheme.fringe
        "row" -> pure ColorScheme.row
        "col" -> pure ColorScheme.col
        _ -> fail "Invalid color scheme name"
        

instance ToJSON Settings
instance FromJSON Settings

defaultSettings :: Settings
defaultSettings = Settings {
    settingsTileType = Fill,
    settingsColorScheme = ColorScheme.fringe,
    settingsGridSize = (4, 4),
    settingsRowHover = 0,
    settingsRefreshRate = 30
}

settingsPath :: IO FilePath
settingsPath = do
    dir <- getXdgDirectory XdgData "15p"
    createDirectoryIfMissing True (dir </> "settings")
    pure (dir </> "settings" </> "settings.json")

loadSettings :: IO Settings
loadSettings = do
    path <- settingsPath
    exists <- doesFileExist path
    if not exists then pure defaultSettings
    else do  
        result <- decodeFileStrict path
        case result of
            Just settings -> pure settings
            Nothing -> pure defaultSettings

storeSettings :: Settings -> IO ()
storeSettings settings = do
    path <- settingsPath
    encodeFile path settings