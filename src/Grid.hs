module Grid where
import qualified Data.Array as Array
import Text.Printf (printf)
import Data.Array ((!), Array, (//))
import qualified Data.Ix as Ix
import Ipair (Ipair, (~-), (~+))
import Control.Monad (when)
import System.Random (randomR, getStdRandom, getStdGen, randomRIO)
import qualified System.Random.Shuffle as Random.Shuffle (shuffle')

type Array2D = Array Ipair Int
data Grid = Grid Ipair Int Array2D -- Size, Move Count, 2d Array

size2D :: Array2D -> Ipair
size2D inArr = snd (Array.bounds inArr) ~+ (1, 1)

grid :: Ipair -> Grid
grid size =
    let (rows, cols) = size
        values = [1..((rows * cols) - 1)] ++ [0] -- 0 is "empty"
        arr = Array.listArray ((0, 0), (rows-1, cols-1)) values
    in Grid size 0 arr
    
_countInversions :: [Int] -> Int
_countInversions [] = 0
_countInversions list = 
    let (x:xs) = list 
    in length (filter (< x) xs)
    
_makeEven :: [Int] -> [Int]
_makeEven (x:y:xs) = y:x:xs
_makeEven _ = error "Could not make permutation even"

_genCorrectPermutation :: Grid -> IO [Int]
_genCorrectPermutation g = do 
    let (Grid size _ _) = g
        (rows, cols) = size
    Random.Shuffle.shuffle' [1..(rows*cols - 1)] (rows * cols - 1) <$> getStdGen -- This linter really be suggesting sum bull shit
    
shuffle :: Grid -> IO Grid
shuffle g = do
    let (Grid size _ _) = g
        (rows, cols) = size
    -- Randomize the permuation of all tiles except for blank (its fixed at the end)
    perm <- _genCorrectPermutation g
    -- -2 because we will be moving it 2 more times before sending it out
    let currentGrid = Grid size (-2) (Array.listArray ((0, 0), (rows-1, cols-1)) (perm ++ [0]))
    
    -- Randomize the starting blank
    randCol <- randomRIO (0, cols-1)
    randRow <- randomRIO (0, rows-1)
    
    -- Move cols then rows
    pure $ move (randRow, randCol) $ move (rows-1, randCol) currentGrid 
    
_mappedPrinter :: Ipair -> Array2D -> Ipair -> IO ()
_mappedPrinter size inArr index = do
    let value = inArr!index
        (_, cols) = size

    if snd index == 0 then putStr "| "
    else putStr " "

    if value == 0 then putStr "  "
    else printf "%2d" value
    when (snd index == cols - 1) $ putStr "|\n"


print2D :: Grid -> IO ()
print2D g = do
    let (Grid size _ inArr) = g
    mapM_ (_mappedPrinter size inArr) (Array.indices inArr)

-- getPos returns the position of the specified value.
getPos :: Int -> Grid -> Ipair
getPos val (Grid _ _ inArr) = fst $ head $ filter (\t -> snd t == val) (Array.assocs inArr)

validPos :: Grid -> Ipair -> Bool
validPos (Grid _ _ inArr) = Ix.inRange (Array.bounds inArr) 

validPosAround :: Ipair -> Grid -> [Ipair]
validPosAround pos g =  
    filter (validPos g) [ 
        pos ~+ (0, 1),
        pos ~+ (0, -1),
        pos ~+ (1, 0),
        pos ~+ (-1, 0)
    ]

move :: Ipair -> Grid -> Grid
move movePos g =
    let blankPos = getPos 0 g
        delta = movePos ~- blankPos
        (moveY, moveX) = movePos
        (blankY, blankX) = blankPos
        (Grid gSize gTotal inArr) = g
        
        updates -- calculate the updates
            | not (validPos g movePos) || delta == (0, 0) || (fst delta /= 0 && snd delta /= 0) -- Either point is the blank or point not inline
                = []
            | fst delta == 0 && snd delta <  0 -- move pos is left of blank space
                = (movePos, 0): [((blankY, i), inArr!(blankY, i-1)) | i <- [(moveX+1)..blankX]]
            | fst delta == 0 && snd delta >  0 -- move pos is right of blank space
                = (movePos, 0): [((blankY, i), inArr!(blankY, i+1)) | i <- [blankX..(moveX-1)]]
            | fst delta <  0 && snd delta == 0 -- move pos is above blank space
                = (movePos, 0): [((i, blankX), inArr!(i-1, blankX)) | i <- [(moveY+1)..blankY]]
            | fst delta >  0 && snd delta == 0  -- move pos is below blank space
                = (movePos, 0): [((i, blankX), inArr!(i+1, blankX)) | i <- [blankY..(moveY-1)]]
            | otherwise = []
    in Grid gSize (gTotal + 1) (inArr // updates)
