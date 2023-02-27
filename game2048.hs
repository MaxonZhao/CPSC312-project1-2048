module Game2048 where

import System.Random
import System.Console.ANSI
import Data.List
import System.IO

-- Define the Board
type Board = [[Int]]
winValue = 2048

-- Lecture abstractions if we want to follow
data State = State InternalState [Action]  -- internal_state available_actions
type Player = State -> Action
type Game = Action -> State -> Result
data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
type InternalState = Board
data Action = UP | DOWN | LEFT | RIGHT | QUIT

-- Function to generate list of all possible coordinates of 2D Matrix of size n in 1D Array format
allCoordinates :: Int -> [(Int, Int)]
allCoordinates size = [(x, y) | x <- [0..(size - 1)], y <- [0..(size - 1)]]    

-- Function to create initial board
-- We define a 0 to be an "empty" piece
-- Note: added functionality for user's size input
createBoard :: Int -> IO Board
createBoard size = 
    let emptyBoard = replicate size (replicate size 0) in
    do
        temp <- insertPiece emptyBoard
        initialBoard <- insertPiece temp
        return initialBoard

-- Function to update a piece value in the board
updatePiece :: Board -> Int -> (Int, Int) -> Board
updatePiece board newVal coord = prefixRows ++ changedRow ++ suffixRows where
    x = fst coord
    y = snd coord
    boardSize = length board
    prefixRows = take x board
    changedRow = ((take y (board!!x)) ++ [newVal] ++ (reverse $ take (boardSize - y - 1) (reverse (board!!x)))):[]
    suffixRows = reverse $ take (boardSize - x - 1) (reverse board)

-- Function to select random element from list (see citations - borrowed function)
choose :: [a] -> IO a
choose list = do
    i <- randomRIO (0, length list - 1)
    return (list !! i)

-- Function to insert a new piece into the Board
insertPiece :: Board -> IO Board
insertPiece board = do
    let emptySquares = getEmptySquares board
    insertVal <- choose [2,2,2,2,2,2,2,2,2,4] -- game rules dictate 90% 2, 10% 4
    insertCoord <- choose emptySquares
    return (updatePiece board insertVal insertCoord)

-- Function to find the coordinates of empty squares where new pieces can be inserted
getEmptySquares :: Board -> [(Int, Int)]
getEmptySquares board = filter (\(x, y) -> (board!!x)!!y == 0) (allCoordinates (length board))

-- Function to check if the board has reached the winning condition 
-- Allow users to specify custom winning value in case 2048 takes too long
checkWin :: Board -> Int -> Bool
checkWin board winValue = filter (\tile -> tile >= winValue) flattenedBoard /= []
    where flattenedBoard = concat board 

-- Function to slide and merge tiles of a row
slideRowLeft :: [Int] -> [Int]
slideRowLeft xs = merged ++ replicate (length xs - length merged) 0
    where merged = merge xs
          merge [] = []
          merge [x] = [x]
          merge (x:y:xs)
            | x == 0 = merge (y:xs)
            | y == 0 = merge (x:xs) 
            | x == y = (x * 2) : merge xs
            | otherwise = x : merge (y:xs)

-- Function to slide and merge tiles of a board for four different directions
slide :: Action -> Board -> Board
slide LEFT board = map slideRowLeft board
slide RIGHT board = map (reverse . slideRowLeft . reverse) board
slide UP board = (transpose . slide LEFT . transpose) board
slide DOWN board =  (transpose . slide RIGHT . transpose) board


-- Function that takes input direction from user
-- nextTurn :: IO Action
nextTurn = do
    input <- getChar
    case lookup input (zip "wasd" [UP, LEFT, DOWN, RIGHT]) of
            Just x -> do putStrLn "" 
                         return x 
            Nothing -> do putStrLn "Use wasd as input" 
                          nextTurn

-- Function to ask user for board size
getBoardSize :: IO Int
getBoardSize = do
    putStrLn "Choose a board size among: 4x4, 6x6, 8x8."
    putStrLn "Enter 4, 6, or 8."
    input <- getChar
    case lookup input (zip "468" [4, 6, 8]) of
            Just x -> do putStrLn ""
                         return x
            Nothing -> do putStrLn "Enter 4, 6, or 8"
                          getBoardSize

-- Function that tests whether the game can proceed
getGameResult :: Board -> Bool
getGameResult board =  canContinue board
    where
        canContinue board = if (allFilled board) then (any (== True) [ if (col + 1 < n && row + 1 < n) 
            then (board!!row!!col == board!!row!!(col+1)) || (board!!row!!col == board!!(row + 1)!!col)
            else if (col + 1 >= n && row + 1 < n) then (board!!row!!col == board!!(row + 1)!!col)
            else if (row + 1 >= n && col + 1 < n) then (board!!row!!col == board!!row!!(col+1))
            else False
            | (row, col) <- coordinates]) else True
        allFilled board = length (filter (== 0) (concat board)) == 0
        coordinates = allCoordinates n
        n = length board 

-- Function encompassing game logic
play :: Board -> IO()
play board = do
    putStr (unlines (map show board))
    if (checkWin board winValue) then
        putStrLn "You Win!"
        else if (getGameResult board) == False then
            putStrLn "You Lose!"
            else do 
                action <- nextTurn
                let newBoard = slide action board
                if (newBoard == board) then
                    play board
                else do
                    newBoard <- insertPiece newBoard
                    play newBoard

-- Main function
main :: IO()
main = do
    hSetBuffering stdin NoBuffering -- Set input buffering mode to 'NoBuffering'
    hSetEcho stdin False -- Disable terminal echoing
    clearScreen -- Clear the screen
    setCursorPosition 0 0 -- Move the cursor to the top-left corner
    
    boardSize <- getBoardSize
    startBoard <- createBoard boardSize
    putStrLn "Please press w, s, a, d to move the board up, down, left, or right"

    play startBoard 

{-Test-}

-- allCoordinates 
ac4 = allCoordinates 4
ac5 = allCoordinates 5

-- getEmptySquares
-- try shouldBeAll which should return list of all coordinates
emptyBoard4by4 = replicate 4 [0,0,0,0]
shouldBeAll = getEmptySquares emptyBoard4by4
-- try nonemptyBoard which should return [(1,0),(1,1),(1,2),(1,3),(3,3)] namely the second row and the bottom right piece
nonemptyBoard = [[1,2,3,4],[0,0,0,0],[1,2,3,4],[1,2,3,0]]
notAll = getEmptySquares nonemptyBoard

-- test updatePiece
setRow2Col2to4 = updatePiece emptyBoard4by4 4 (2,2)
setRow0Col0to2 = updatePiece emptyBoard4by4 2 (0,0)
setNonEmpty = updatePiece nonemptyBoard 128 (1,1)
setFull = updatePiece testBoard2d4 999 (1,1)

-- test Choose
chosenCoordinate = choose notAll
chosenValue = choose [2,2,2,2,2,2,2,2,2,4]

-- test insertPiece
insertEmpty = insertPiece emptyBoard4by4
insertNonEmpty = insertPiece nonemptyBoard 
insertFull = insertPiece testBoard2d4 -- Should throw exception

-- test createBoard
-- Should have two randomly placed pieces of either 2 or 4 every time
newBoard = createBoard

-- test slide functions against these matrices
-- try slide LEFT testBoard2d1, the expected result is [[0, 4], [0, 8]]
testBoard2d1 :: Board = [[2, 2], [4, 4]]
testBoard2d2 :: Board = [[2, 4], [2, 4]]
testBoard2d3 :: Board = [[0, 2], [2, 4]]

-- test getGameResult function against the following 3 matrices
-- try getGameResult testBoard2d4
-- test with the first matrix should return false, meaning that the game has ended (cannot continue)
-- test with the second matrix should return true
-- test with the third matrix should return true
testBoard2d4 :: Board = [[2, 16, 32, 4], [4, 8, 2, 8], [16, 2, 32, 4], [2, 4, 8, 2]]
testBoard2d5 :: Board = [[2, 16, 32, 4], [4, 8, 2, 8], [16, 2, 32, 8], [2, 4, 8, 2]]
testBoard2d6 :: Board = [[2, 16, 32, 4], [4, 8, 2, 8], [16, 2, 0, 4], [2, 4, 8, 2]]

{-Resources/Citation-}
--https://en.wikipedia.org/wiki/2048_(video_game)
--https://github.com/dwnusbaum/Haskell-2048
--https://github.com/gregorulm/h2048
--https://discourse.haskell.org/t/how-to-install-modules/1363/2
--https://stackoverflow.com/questions/42544101/show-matrix-function-in-haskell
--https://stackoverflow.com/questions/6082090/haskell-deriving-show-for-custom-type
--http://zvon.org/other/haskell/Outputprelude/filter_f.html
--https://gaming.stackexchange.com/questions/170665/what-is-the-probability-of-4
--https://stackoverflow.com/questions/27715033/modifying-elements-of-haskell-2d-array
--https://wiki.haskell.org/Global_variables
--https://hackage.haskell.org/package/ansi-terminal-0.11.4/docs/System-Console-ANSI.html
            
