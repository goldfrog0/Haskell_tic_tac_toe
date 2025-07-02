module Main where

import Data.Maybe ( isJust
                  , isNothing)
import Text.Read ( readMaybe )

data Player = X | O
  deriving (Eq, Show)

type Cell = Maybe Player
type Board = [Cell]

-- Initial gamestate for the game, an empty board.
emptyBoard :: Board
emptyBoard = replicate 9 Nothing

-- prints the board state based on what is present in the board. BOARD LENGTH MUST BE DIVISIBLE BY THREE
showBoard :: Board -> IO ()
showBoard ls = putStrLn $ (++) "_____________\n" $ aux $ stringifyBoard ls -- prints top line of board, and the rest
  where
    aux [] = ""                                               -- conider factoring out this function to steal functionality
    aux [_] = ""
    aux (a:b:c:rest) = "|" ++ a ++                            -- dodgy multiple of three condition
                       "|" ++ b ++                            -- the rest of the code makes the bars and spacing
                       "|" ++ c ++ "|\n" ++
                       "_____________\n" ++ aux rest
    aux _ = error "missalignment: possibly wrong board size"  -- error detected

-- turns the board into a string to make it "easier" to add into the printed board.
stringifyBoard :: Board -> [String]
stringifyBoard [] = []                                        -- base case
stringifyBoard (Nothing:rest) = "   " : stringifyBoard rest   -- empty cell
stringifyBoard ((Just X):rest)  = " X " : stringifyBoard rest -- x mark
stringifyBoard ((Just O):rest)  = " O " : stringifyBoard rest -- o mark

-- makes move on board by checking if spot is not taken
-- ONLY USE BOARD OF LENGTH 9 for safety
makeMove :: Board -> Int -> Player -> Maybe Board
makeMove currentBoard position player            -- takes a board, index and current player
  | position < 0 || position > 8 = Nothing       -- if the index is out of range, Nothing
  | isJust $ currentBoard !! position = Nothing  -- if the current cell is taken, Nothing
  | otherwise = Just $ insertAt (Just player) currentBoard position  -- use insertAt to insert mark at position

-- replaces element of list at an index
-- if its out of range, does nothing
insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt item (_:xs) 0  = item:xs
insertAt item (x:xs) position = x:insertAt item xs (position -1)

-- handles interaction for a players turn.
playerTurn :: Board -> Player -> IO Board
playerTurn board player = do
  putStrLn $ "Player: " ++ show player                    --prompts player
           ++ " Please enter the position of your move"
  input <- getLine                       -- get cell number from player
  case readMaybe input :: Maybe Int of   -- tries to get an integer from user
    Just n | 1 <= n && n <= 9 ->         -- int is between 0 - 9, continue
        case makeMove board idx player of   -- tries to place mark at position given
          Just newState -> return newState  -- if succesful, return new state
          Nothing -> do                     -- if not throw error.
            putStrLn "That position is taken, please try again."
            playerTurn board player         -- recurse
        where idx = n - 1
    _anyotherFailure  -> do                 --
      putStrLn "That is not a valid input on the board, please try again."
      playerTurn board player

checkLines :: [[Int]]
checkLines = [[0,1,2],
              [3,4,5],
              [6,7,8],
              [0,3,6],
              [1,4,7],
              [2,5,8],
              [0,4,8],
              [2,4,6]]

-- checks board for a win on predefined lines
checkWin :: Board -> Maybe Player
checkWin board = aux board checkLines
  where
    aux :: Board -> [[Int]] -> Maybe Player
    aux _ [] = Nothing       -- base case, empty lines list, return Nothing, ie no win found
    aux input (indices:xs)
      | xWin input indices = Just X  -- checks for x win on current line
      | oWin input indices = Just O  -- checks for o win on current line
      | otherwise       = aux board xs   -- no win found, recurse starting at next line

-- checks board for draw
isDraw :: Board -> Bool
isDraw board =
  all isJust board &&   -- checks that current board is filled
  isNothing (checkWin board) -- checks no one has won isNothin

-- checks a list of cell positions for all Xs or Os
xWin :: Board -> [Int] -> Bool
xWin board  = all $ (== Just X).(board !!)
oWin :: Board -> [Int] -> Bool
oWin board  = all $ (== Just X).(board !!)

-- test board for debugging, this one is a draw
testBoardDraw :: Board
testBoardDraw = [ Just X, Just O, Just X
                  , Just X, Just O, Just O
                  , Just O, Just X, Just X
                  ]

-- test board for debugging, this one is a win
testBoardxwin :: Board
testBoardxwin = [ Just X, Nothing, Just O
                  , Just X, Just X, Just X
                  , Just O, Nothing, Nothing
                  ]

-- use to change player, swaps x to o and vice versa
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

-- game loop logic
gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
  showBoard board    -- prints board at start of each iteration
  case checkWin board of
    (Just X) -> do putStrLn "Congratulations, player X wins"  -- If a player has won, print appropriate win message
                   showBoard board  -- show final board state
    (Just O) -> do putStrLn "Congratulations player O wins" -- same as above
                   showBoard board
    Nothing  -> if isDraw board then putStrLn "The game is a draw!!! Womp Womp" >> showBoard board  else -- draw condition check
      do nextBoard <- playerTurn board player -- if not a draw, player makes a move, and we recurse
         gameLoop nextBoard (switchPlayer player)

main :: IO ()
main = gameLoop emptyBoard X
