module Main where
import Brick
import Brick (customMain)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Game 
import State
import Grid (grid, shuffle)

testGrid = grid (4, 4)

main :: IO ()   
main = do
    randomGrid <- shuffle testGrid
    let initialState = State.GameState { gameScene = PlayScene, gameStartTime = pure 0, gameGrid = randomGrid}
    
    -- 2. Create the Vty builder configuration
    let buildVty = VCP.mkVty V.defaultConfig
    
    -- 3. Build the Vty instance manually
    initialVty <- buildVty
    
    -- 4. THE OVERRIDE: Forcibly tell Vty to send the Mouse activation code
    -- This overrides whatever vty *thinks* Ghostty is capable of.
    V.setMode (V.outputIface initialVty) V.Mouse True
    
    -- 5. Boot the app using customMain instead of defaultMain
    -- The 'Nothing' means we are not using a custom background event channel.
    finalState <- customMain initialVty buildVty Nothing appConfig initialState
    
    putStrLn "Game gracefully closed."