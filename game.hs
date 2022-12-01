module Game where
import Prelude
import Data.List ( transpose )
import System.Random
import Control.Monad

data Move = UP
          | DOWN
          | LEFT
          | RIGHT

type Row = [Int]
type Board = [[Int]]

bSize :: Int
bSize = 4

distVals :: [Int]
distVals = [2,4,2,2,2,2,4,2]

newBoard :: Int -> Board
newBoard n = replicate n (replicate n 0)

len :: Num t => [a] -> t
len = foldl (\ n x -> n + 1) 0

-- Filter 0 out and 'stacks' elements, combining equal ones.
stack :: (Eq a, Num a) => [a] -> [a]
stack xs = stacked ++ zeroes
          where zeroes = replicate (len xs - len stacked) 0
                stacked = case filter (/= 0) xs of
                    [] -> []
                    [x] -> [x]
                    (y:x:xs) -> if x == y
                         then x + y : stack xs
                         else y : stack (x:xs)

-- Apply 'stack' to each Row on the Board and transposes/reverses accordly.
moveTo :: Move -> Board -> Board
moveTo m = case m of
          LEFT -> map stack
          RIGHT -> map (reverse . stack . reverse)
          UP -> transpose . moveTo LEFT . transpose
          DOWN -> transpose . moveTo RIGHT . transpose

-- Return a set of coordinates with zeroes on
emptyTiles :: (Eq a, Num a) => [[a]] -> [(Int, Int)]
emptyTiles board = filter (\(x,y) -> board !! x !! y == 0) coords
        where   coords = [(x,y) | x <- [0..n-1], y <- [0..n-1]]
                n = len board

-- Set specified Value at position on Board
setValueAt :: Board -> (Int,Int) -> Int -> Board
setValueAt board (x,y) value = take x board ++ placedRow ++ drop (x+1) board
                    where placedRow = [take y (board !! x) ++ [value] ++ drop (y+1) (board !! x)]

newVal :: [a] -> IO a
newVal vs = do 
        x <- randomRIO (0, len vs - 1)
        return (vs !! x)

turn :: Board -> Move -> IO Board
turn board move =
        do
        board <- pure(moveTo move board)
        let empty = emptyTiles board
        pos <- randomRIO(0, length empty - 1) >>= \x -> return (empty !! x)
        val <- newVal distVals
        return(setValueAt board pos val)

startBoard :: IO Board
startBoard = do
                board <- pure (newBoard bSize)
                let empty = emptyTiles board
                pos <- randomRIO (0, length empty - 1) >>= \x -> return (empty !! x)
                board <- pure(setValueAt board pos 2)
                let empty = emptyTiles board
                pos <- randomRIO (0, length empty - 1) >>= \x -> return (empty !! x)
                pure(setValueAt board pos 2)