module Main where
import qualified Data.Foldable as Foldable 
import qualified Data.Array as Array 
import Data.Array ((!), Array)

rows = 4
cols = 4
values = [1..((rows * cols) - 1)] ++ [0] -- 0 is "empty"
arr = Array.listArray ((0, 0), (3, 3)) values


printHelper :: Array.Array (Int, Int) Int -> (Int, Int) -> IO ()
printHelper inArr index  
    | snd index == rows - 1 = do
        putStr " "
        putStr (show (inArr!index))
        putStr " |\n"
    | snd index == 0 = do 
        putStr "| "
        putStr (show (inArr!index))
    | otherwise = do
        putStr " "
        putStr (show (inArr!index))


printArray2D :: Array (Int, Int) Int -> IO ()
printArray2D inArr = do
    mapM_ (printHelper inArr) (Array.indices inArr)
    

main :: IO ()
main = do
    putStrLn "Yea baby"
    print (Array.elems arr)
    printArray2D arr