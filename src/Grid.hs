module Grid where

import Ipair (Ipair, (~-), (~+))

import Control.Monad (when)
import Text.Printf (printf)
import Data.Vector.Unboxed ((!), Vector, (//), generate, find, findIndex, elemIndex, unsafeUpd, toList)
import qualified Data.Ix as Ix
import System.Random (getStdGen, randomRIO)
import qualified System.Random.Shuffle as Random.Shuffle (shuffle')
import Data.Maybe (isNothing, fromJust)
import Data.List.Split (chunksOf)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as VU
import Data.Foldable (Foldable(foldl'))

type Vector2D = Vector Int
data Grid = Grid {
    gridSize :: Ipair,
    gridMoveCount :: Int,
    gridVec :: Vector2D,
    gridMDCache :: Int -- Cached value of Manhattan Distance to reduce runtime
    -- gridLCCache :: Int
}

instance Show Grid where
    show g =
        let (rows, cols) = gridSize g
        in  printf "Grid %d" (gridMoveCount g) ++  -- Grid with move count  -- Grid with move count
              -- Grid with move count
            "\n" ++
            unlines (map (unwords . map show) (chunksOf cols (toList (gridVec g)))) -- 2d vector

instance Eq Grid where
    (==) g1 g2 = gridMDCache g1 == gridMDCache g2 && gridVec g1 == gridVec g2

genOrdered2DVector :: Ipair -> Vector2D
genOrdered2DVector bounds =
    let (rows, cols) = bounds
        size = rows * cols
    in generate (rows * cols) ( \i ->  if i == size - 1 then 0 else i + 1 )

grid :: Ipair -> Grid
grid size =
    let arr = genOrdered2DVector size
    in Grid {
        gridSize = size,
        gridMoveCount = 0,
        gridVec = arr,
        gridMDCache = 0
        -- gridLCCache = 0
    }

-- Expand the 1D index to a 2D index
expandIndex :: Grid -> Int -> Ipair
expandIndex g i =
    let (rows, cols) = gridSize g
    in (i `div` cols, i `mod` cols)

-- Squash the 2D index to a 1D index
squashIndex :: Grid -> Ipair -> Int
squashIndex g (row, col) =
    let (rows, cols) = gridSize g
    in row * cols + col

-- Get the tile at the index (1D)
getTile' :: Grid -> Int -> Int
getTile' g i = gridVec g ! i

-- Get the tile at the 2D index
getTile :: Grid -> Ipair -> Int
getTile g (row, col) =  gridVec g ! squashIndex g (row, col)

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
    let perm' = perm ++ [0]
        currentGrid = Grid size 0 (generate (rows * cols) (perm' !!)) 0 

    -- Randomize the starting blank
    randCol <- randomRIO (0, cols-1)
    randRow <- randomRIO (0, rows-1)

    -- Move cols then rows
    let shuffledGrid = move (randRow, randCol) $ move (rows-1, randCol) currentGrid
    pure $ shuffledGrid { 
        gridMoveCount = 0, 
        gridMDCache = manhattanDistance shuffledGrid
        -- gridLCCache = linearConflict shuffledGrid 
    } -- Reset move count after shuffling


-- getPos returns the position of the specified value.
getPos :: Int -> Grid -> Ipair
getPos val g =
    let vec = gridVec g
        index = elemIndex val vec
        (rows, cols) = gridSize g
    in maybe (error "Value not found")  (expandIndex g)  index

getOriginalPos :: Grid -> Int -> Ipair
getOriginalPos g val =
    let (rows, cols) = gridSize g
    in  if val == 0 then (rows - 1, cols - 1)
        else ((val - 1) `div` cols, (val - 1) `mod` cols)

validPos :: Grid -> Ipair -> Bool
validPos (Grid (rows, cols) _ _ _)  pos =
    let (row, col) = pos
    in  row >= 0 && row < rows && col >= 0 && col < cols

-- Checks whether or not the position will change the grid or not
validMovePos :: Grid -> Ipair -> Bool
validMovePos g movePos =
    let blankPos = getPos 0 g
        delta = movePos ~- blankPos
    in  validPos g movePos -- Must be inside the possible index
        && delta /= (0, 0) -- Not the blank position
        && (fst delta == 0 || snd delta == 0) -- Must be inline with an axis of the blank tile

getIntersectingRow :: Grid -> Ipair -> [Ipair]
getIntersectingRow g pos =
    let (rows, cols) = gridSize g
        (currentRow, currentCol) = pos
    in  [(row, currentCol) | row <- [0..rows-1]]

getIntersectingCol :: Grid -> Ipair -> [Ipair]
getIntersectingCol g pos =
    let (rows, cols) = gridSize g
        (currentRow, currentCol) = pos
    in  [(currentRow, col) | col <- [0..cols-1]]


-- Move a tile with the pos of Ipair
move :: Ipair -> Grid -> Grid
move movePos g =
    let blankPos = getPos 0 g
        delta = movePos ~- blankPos
        (moveY, moveX) = movePos
        (blankY, blankX) = blankPos
        (Grid _ gTotal vec _) = g
        squashIndex' = squashIndex g
        movePos' = squashIndex' movePos

        updates -- calculate the updates
            | not (validMovePos g movePos) = []
            | fst delta == 0 && snd delta <  0 -- move pos is left of blank space
                = (movePos', 0): [(squashIndex' (blankY, i), vec ! squashIndex' (blankY, i-1)) | i <- [(moveX+1)..blankX]]
            | fst delta == 0 && snd delta >  0 -- move pos is right of blank space
                = (movePos', 0): [(squashIndex' (blankY, i), vec ! squashIndex' (blankY, i+1)) | i <- [blankX..(moveX-1)]]
            | fst delta <  0 && snd delta == 0 -- move pos is above blank space
                = (movePos', 0): [(squashIndex' (i, blankX), vec ! squashIndex' (i-1, blankX)) | i <- [(moveY+1)..blankY]]
            | fst delta >  0 && snd delta == 0  -- move pos is below blank space
                = (movePos', 0): [(squashIndex' (i, blankX), vec ! squashIndex' (i+1, blankX)) | i <- [blankY..(moveY-1)]]
            | otherwise = error "Your logic is invalid dumbass"


    in  if null updates then g
        else g {
            gridMoveCount = gTotal + 1,
            gridVec = unsafeUpd vec updates,
            gridMDCache = gridMDCache g -- The current cache
                          - sum (map (\(index, _) -> getManhattanDistanceOfTile g index (getTile' g index)) updates) -- Minus the old changed tiles
                          + sum (map (uncurry (getManhattanDistanceOfTile g)) updates) -- Plus the new changed tiles
            -- gridLCCache =
            --     let changedRows = map (getIntersectingRow g . expandIndex g . fst) updates
            --         changedCols = map (getIntersectingCol g . expandIndex g . fst) updates
            --         (rows, cols) = gridSize g
            --         newGrid = g { gridVec = unsafeUpd vec updates }
            --         getRowLC currentGrid = sum $
            --             map (snd . foldl' (\msg (currentRow, currentCol) ->
            --                     _conflictReducer currentGrid
            --                         (\val -> (val - 1) `div` cols == currentRow)
            --                         msg
            --                         (currentRow, currentCol)
            --                     ) (-1, 0)
            --                 ) changedRows
            --         getColLC currentGrid = sum $
            --             map (snd . foldl' (\msg (currentRow, currentCol) ->
            --                     _conflictReducer currentGrid
            --                         (\val -> (val - 1) `mod` cols == currentCol)
            --                         msg
            --                         (currentRow, currentCol)
            --                     ) (-1, 0)
            --                 ) changedCols
                    
            --     in gridLCCache g -- The current cache
            --         + getRowLC newGrid - getRowLC g
            --         + getColLC newGrid - getColLC g
                    
        }

getMovePosFromOffset :: Ipair -> Grid -> Ipair
getMovePosFromOffset os g = getPos 0 g ~+ os

-- move a block offsetted from the blank tile by Ipair
offset :: Ipair -> Grid -> Grid
offset movePos g = move (getMovePosFromOffset movePos g) g

validOffsetPos :: Grid -> Ipair -> Bool
validOffsetPos g os =
    let blankPos = getPos 0 g
        movePos = blankPos ~+ os
        delta = movePos ~- blankPos
    in  validPos g movePos -- Must be inside the possible index
        && delta /= (0, 0) -- Not the blank position
        && (fst delta == 0 || snd delta == 0) -- Must be inline with an axis of the blank tile

isSolved :: Grid -> Bool
isSolved g = gridMDCache g == 0

getManhattanDistanceOfTile :: Grid -> Int -> Int -> Int
getManhattanDistanceOfTile grid index val =
    if val == 0 then 0
    else
        let originalPos = Grid.getOriginalPos grid val
            delta = originalPos ~- Grid.expandIndex grid index
        in (abs . fst) delta + (abs . snd) delta

manhattanDistance :: Grid -> Int
manhattanDistance grid = VU.foldl' (+) 0 $ VU.imap (getManhattanDistanceOfTile grid) (Grid.gridVec grid)

-- returns the maximum value found of the row / column and the current conflict count
-- check is boolean function to check whether the value at the provided index is at the correct row or col
_conflictReducer :: Grid -> (Int -> Bool) -> (Int, Int) -> Ipair -> (Int, Int)
_conflictReducer grid check (maxVal, conflictCount) index =
    let val = Grid.getTile grid index
    in
        if val == 0 || not (check val) then (maxVal, conflictCount)
        else if val > maxVal then (val, conflictCount)
        else (maxVal, conflictCount + 2)

-- Incase this breaks (which it probably will because haskell is an ugly unreadable language)
-- Go here to see what you did wrong: https://github.com/MilanPecov/15-Puzzle-Solvers/blob/master/fifteen_puzzle_solvers/services/puzzle/heuristic.py
linearConflict :: Grid -> Int
linearConflict g =
    let (rows, cols) = Grid.gridSize g
        _conflictReducer' = _conflictReducer g
        rowConflicts = sum $
            map (\currentRow -> snd $ foldl' (\msg currentCol ->
                    _conflictReducer'
                        (\val -> (val - 1) `div` cols == currentRow)
                        msg
                        (currentRow, currentCol)
                    ) (-1, 0) [0..cols-1]
                ) [0..rows-1]

        colConflicts = sum $
            map (\currentCol -> snd $ foldl' (\msg currentRow ->
                    _conflictReducer'
                        (\val -> (val - 1) `mod` cols == currentCol)
                        msg
                        (currentRow, currentCol)
                    ) (-1, 0) [0..rows-1]
                ) [0..cols-1]
    in rowConflicts + colConflicts