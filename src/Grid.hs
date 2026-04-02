module Grid where

import Ipair (Ipair, (~-), (~+))

import Control.Monad (when)
import qualified Data.Array as Array
import Text.Printf (printf)
import Data.Array ((!), Array, (//))
import qualified Data.Ix as Ix
import System.Random (getStdGen, randomRIO)
import qualified System.Random.Shuffle as Random.Shuffle (shuffle')

type Array2D = Array Ipair Int
data Grid = Grid {   
    gridSize :: Ipair,
    gridMoveCount :: Int,
    gridArr :: Array2D
} deriving Show

size2D :: Array2D -> Ipair
size2D inArr = snd (Array.bounds inArr) ~+ (1, 1)

_genOrdered2DArray :: Ipair -> Array2D
_genOrdered2DArray size = 
    let (rows, cols) = size
        values = [1..((rows * cols) - 1)] ++ [0] -- 0 is "empty"
    in Array.listArray ((0, 0), (rows-1, cols-1)) values

grid :: Ipair -> Grid
grid size =
    let arr = _genOrdered2DArray size
    in Grid {
        gridSize = size,
        gridMoveCount = 0,
        gridArr = arr
    }
    
_countInversions :: [Int] -> Int
_countInversions [] = 0
_countInversions list = 
    let (x:xs) = list 
    in length (filter (< x) xs) + _countInversions xs
    
_makeEven :: [Int] -> [Int]
_makeEven (x:y:xs) = y:x:xs
_makeEven _ = error "Could not make permutation even"

_genCorrectPermutation :: Grid -> IO [Int]
_genCorrectPermutation g = do 
    let (rows, cols) = gridSize g
    perm <- Random.Shuffle.shuffle' [1..(rows*cols - 1)] (rows * cols - 1) <$> getStdGen -- This linter really be suggesting sum bull shit
    if even (_countInversions perm) then pure perm
    else pure (_makeEven perm)
    
shuffle :: Grid -> IO Grid
shuffle g = do
    let size = gridSize g 
        (rows, cols) = size
    -- Randomize the permuation of all tiles except for blank (its fixed at the end)
    perm <- _genCorrectPermutation g
    -- -2 because we will be moving it 2 more times before sending it out
    let currentGrid = Grid size 0 (Array.listArray ((0, 0), (rows-1, cols-1)) (perm ++ [0]))
    
    -- Randomize the starting blank
    randCol <- randomRIO (0, cols-1)
    randRow <- randomRIO (0, rows-1)
    
    -- Move cols then rows
    let shuffledGrid = move (randRow, randCol) $ move (rows-1, randCol) currentGrid 
    pure $ shuffledGrid { gridMoveCount = 0 } -- Reset move count after shuffling
    
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
    let size = gridSize g 
        inArr = gridArr g
    mapM_ (_mappedPrinter size inArr) (Array.indices inArr)

-- getPos returns the position of the specified value.
getPos :: Int -> Grid -> Ipair
getPos val (Grid _ _ inArr) = fst $ head $ filter (\t -> snd t == val) (Array.assocs inArr)

getOriginalPos :: Int -> Grid -> Ipair
getOriginalPos val g = 
    let (rows, cols) = gridSize g
    in  if val == 0 then (rows - 1, cols - 1)
        else ((val - 1) `div` cols, (val - 1) `mod` cols)
    
validPos :: Grid -> Ipair -> Bool
validPos g = Ix.inRange $ Array.bounds $ gridArr g

validPosAround :: Ipair -> Grid -> [Ipair]
validPosAround pos g =  
    filter (validPos g) [ 
        pos ~+ (0, 1),
        pos ~+ (0, -1),
        pos ~+ (1, 0),
        pos ~+ (-1, 0)
    ]

-- Move a tile with the pos of Ipair
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
        
        updatedArr = inArr // updates
    in  if updatedArr == inArr then g
        else g { gridMoveCount = gTotal + 1, gridArr = updatedArr }

-- move a block offsetted from the blank tile by Ipair
offset :: Ipair -> Grid -> Grid
offset movePos g = move (getPos 0 g ~+ movePos) g

isSolved :: Grid -> Bool
isSolved g = gridArr g == _genOrdered2DArray (gridSize g)