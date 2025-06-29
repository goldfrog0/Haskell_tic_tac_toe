module Main where

import Data.Maybe
import Text.Read


data Player = X | O
  deriving (Eq, Show)

type Cell = Maybe Player
type Board = [Cell]

emptyBoard :: Board
emptyBoard = replicate 9 Nothing

showBoard :: Board -> IO ()
showBoard ls = putStrLn $ "_____________\n" ++ (aux $ stringifyBoard ls)
  where
    aux [] = ""
    aux [_] = ""
    aux (a:b:c:rest) = "|" ++ a ++
                       "|" ++ b ++
                       "|" ++ c ++ "|\n" ++
                       "_____________\n" ++ aux rest
    aux _ = error "missalignment: possibly wrong board size"

stringifyBoard :: Board -> [String]
stringifyBoard [] = []
stringifyBoard (Nothing:rest) = "   " : stringifyBoard rest
stringifyBoard ((Just X):rest)  = " X " : stringifyBoard rest
stringifyBoard ((Just O):rest)  = " O " : stringifyBoard rest

makeMove :: Board -> Int -> Player -> Maybe Board
makeMove currentBoard position player
  | position < 0 || position > 8 = Nothing
  | isJust $ currentBoard !! position = Nothing
  | otherwise = Just $ insertAt (Just player) currentBoard position

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt item (_:xs) 0  = item:xs
insertAt item (x:xs) position = x:insertAt item xs (position -1)

playerTurn :: Board -> Player -> IO Board
playerTurn board player = do
  putStrLn $ "Player: " ++ show player
           ++ " Please enter the position of your move"
  input <- getLine
  case readMaybe input :: Maybe Int of
    Just n | 1 <= n && n <= 9 ->
        case makeMove board idx player of
          Just newState -> return newState
          Nothing -> do
            putStrLn "That position is taken, please try again."
            playerTurn board player
        where idx = n - 1
    _  -> do
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

checkWin :: Board -> Maybe Player
checkWin board = aux board checkLines
  where
    aux :: Board -> [[Int]] -> Maybe Player
    aux _ [] = Nothing
    aux input (indices:xs)
      | xWin input indices = Just X
      | oWin input indices = Just O
      | otherwise       = aux board xs

isDraw :: Board -> Bool
isDraw board = all isJust board &&
  case checkWin board of
                 Just _  -> False
                 Nothing -> True

--TODO refactor code with any all pattern
xWin :: Board -> [Int] -> Bool
xWin board indices = all (== Just X) $ map (board !!) indices
oWin :: Board -> [Int] -> Bool
oWin board indices = all (== Just O) $ map (board !!) indices

testBoardDraw :: Board
testBoardDraw = [ Just X, Just O, Just X
                  , Just X, Just O, Just O
                  , Just O, Just X, Just X
                  ]

testBoardxwin :: Board
testBoardxwin = [ Just X, Nothing, Just O
                  , Just X, Just X, Just X
                  , Just O, Nothing, Nothing
                  ]

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X


gameLoop :: Board -> Player -> IO ()
gameLoop board player = do
  showBoard board
  case checkWin board of
    (Just X) -> do putStrLn "Congratulations, player X wins"
                   showBoard board
    (Just O) -> do putStrLn "Congratulations player O wins"
                   showBoard board
    Nothing  -> if isDraw board then putStrLn "The game is a draw!!! Womp Womp" >> showBoard board  else
      do nextBoard <- playerTurn board player
         gameLoop nextBoard (switchPlayer player)

main :: IO ()
main = gameLoop emptyBoard X
