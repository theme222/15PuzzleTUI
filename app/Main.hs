module Main where
import qualified Grid 
import Grid (Grid)
import qualified Ipair
import Ipair ((~+), (~-), Ipair)
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))

testGrid :: Grid
testGrid = Grid.grid (4, 4)

getIntent :: String -> Grid -> Ipair
getIntent "w" g = Grid.getPos 0 g ~+ (1,  0)
getIntent "s" g = Grid.getPos 0 g ~+ (-1, 0)
getIntent "a" g = Grid.getPos 0 g ~+ (0,  1)
getIntent "d" g = Grid.getPos 0 g ~+ (0, -1)
getIntent  _  g = Grid.getPos 0 g -- Effectively do nothing


loop :: Grid -> IO () 
loop currentGrid = do
    Grid.print2D currentGrid
    putStr "Input next action (w, a, s, d): "
    action <- getLine
    loop (Grid.move (getIntent action currentGrid) currentGrid)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    loop testGrid