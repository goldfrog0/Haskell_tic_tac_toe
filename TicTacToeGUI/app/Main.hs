module Main where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


data Player = X | O
  deriving (Eq, Show)
-- This what functions are looking at in terms of players the type handling the population of the cells

type Cell = Maybe Player
-- cells on the board can be a players mark, or empty (Nothing)

type Board = [Cell]
-- The Board is a list that is meant to always be 9 units long, corresponding to the nine cells on a tic tac toe board.

data GameState = GameState
  { board         :: Board
  , currentPlayer :: Player
  , gameOver      :: Maybe (Maybe Player)}
-- The game state has a board, representing the current board state, the current player, and a game over condition.
-- Maybe Player _ <- represents a player win.
-- Maybe Nothing  <- represents a draw
-- Nothing        <- represents no end condition

-- GameState for debugging
testGameState :: GameState
testGameState = GameState
  { board         = testBoardDraw
  , currentPlayer = X
  , gameOver      = Nothing}

-- Initial gamestate for the game
initialState :: GameState
initialState = GameState
  { board         = emptyBoard
  , currentPlayer = X
  , gameOver      = Nothing}

drawGame :: GameState -> Picture -- create a Picture based on the current game state
drawGame state = pictures [drawGrid, drawMarks (board state), drawGameOverMessage (gameOver state)]

-- create the Picture for the gridlines only, does not change.
drawGrid :: Picture
drawGrid = pictures $ map (color white)
  [ horizontalLine1
  , horizontalLine2
  , verticalLine1
  , verticalLine2 ]
  where
    horizontalLine1 = line [(-300, 100), (300, 100)]
    horizontalLine2 = line [(-300, -100), (300, -100)]
    verticalLine1   = line [(100, -300), (100, 300)]
    verticalLine2   = line [(-100, -300), (-100, 300)]

-- Converts a grid index, to a grid coordinate.
intToGrid :: Int -> (Float, Float) -- Takes an integer and gives a grid position on where to draw the mark.
intToGrid 0 = (-200, 200)
intToGrid 1 = (0, 200)
intToGrid 2 = (200, 200)
intToGrid 3 = (-200, 0)
intToGrid 4 = (0, 0)
intToGrid 5 = (200, 0)
intToGrid 6 = (-200, -200)
intToGrid 7 = (0, -200)
intToGrid 8 = (200, -200)
intToGrid _ = (0,0)

-- Depending on the position, and player, draw a mark on the board.
drawMark :: Int -> Maybe Player -> Picture
drawMark cell (Just X) = drawX $ intToGrid cell -- If given X, draw an X on cell
drawMark cell (Just O) = drawO $ intToGrid cell -- If given O, draw an O on cell
drawMark _    Nothing  = Blank                  -- If no player, draw a blank.

-- Defines the lines for the X "sprite"
xPicture :: Picture
xPicture = pictures [diagonal1, diagonal2]
  where
    diagonal1 = line [(-60, -60),(60, 60)]
    diagonal2 = line [(-60, 60),(60, -60)]

-- draws an X on the grid, at coordinate tuple.
drawX :: (Float, Float) -> Picture
drawX (a, b) = translate a b $ color red xPicture

-- draws an O on the grid, at coordinate tuple.
drawO :: (Float, Float) -> Picture
drawO (a, b) = translate a b (color blue (thickCircle 60 5))

-- Given a board, generate the pictures for the grid.
drawMarks :: Board -> Picture
drawMarks currentboard = pictures (zipWith drawMark [0..] currentboard) -- make list of pictures

-- Prints game over messages
drawGameOverMessage :: Maybe (Maybe Player) -> Picture
drawGameOverMessage Nothing = blank
drawGameOverMessage (Just Nothing)  = placeText "The game is a Draw, WOMP WOMP!!!"
drawGameOverMessage (Just (Just X)) = placeText "Congratulations, X wins"
drawGameOverMessage (Just (Just O)) = placeText "Congratulations, O wins"

-- places text scaled, and at constant coordinates.
placeText :: String -> Picture
placeText str = scale 0.3 0.3 $ translate (-700) 1050 $ color white $ text str

-- handles user actions in the game
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) state -- Catches mouse left click
  | isNothing $ gameOver state = tryPlacingMarkAt x y state  -- if the game is a not a draw or win, try player move
  | otherwise = state       --Redundant default state
handleEvent _ state = state --Ignores any other input

-- function to handle placing marks at coordinates to be supplied by handle event function.
tryPlacingMarkAt :: Float -> Float -> GameState -> GameState
tryPlacingMarkAt x y state = -- accepting a coordinate x y here
  case positionToCell x y of -- case match on indices that are in the counds of the grid
    Nothing -> state         -- Not in bounds -> dont change state
    (Just idx) -> case makeMove (board state) idx (currentPlayer state) of -- in bounds, try to place mark on grid
      Nothing -> state                                -- looking for Maybe Board -> cell is taken -> dont change state
      Just newBoard ->                                -- if make move is succesful
        let newGameOver = case checkWin newBoard of   -- checking for end condition to modify state
              Just winner -> Just (Just winner)       -- its a Just Player, we have a win.
              Nothing     -> if isDraw newBoard       -- check its nothing Check for Draw
                             then Just Nothing        -- draw -> Just Nothing
                             else Nothing             -- not draw -> Nothing
        in state { board = newBoard                   -- updating newGameOver in state
                 , currentPlayer = switchPlayer (currentPlayer state)
                 , gameOver = newGameOver }

-- Takes x y coordinates, and determines which cell the coordinate is
positionToCell :: Float -> Float -> Maybe Int
positionToCell x y
  | abs x > 300 || abs y > 300 = Nothing    -- check if in grid -> might be redundant DONE check redundancy
  | otherwise = case (col, row) of          -- In grid? -> determine cell clicked on
      (Just c, Just r) -> Just (r * 3 + c)  -- gives us the cell number
      _                -> Nothing           -- fallback
  where
    col                      -- determine the column
      | x < -100  = Just 0
      | x <  100  = Just 1
      | x <= 300  = Just 2
      | otherwise = Nothing

    row                      -- determine the row
      | y >   100 = Just 0
      | y >  -100 = Just 1
      | y >= -300 = Just 2
      | otherwise = Nothing

-- initial board state
emptyBoard :: Board
emptyBoard = replicate 9 Nothing

-- makes a move depending on cell of board and current player
-- potential to refactor, checking if cell is Nothing or Player instead of using insertAt function
makeMove :: Board -> Int -> Player -> Maybe Board
makeMove currentBoard position player
  | position < 0 || position > 8 = Nothing       -- checking given index is in the board
  | isJust $ currentBoard !! position = Nothing  -- if cell is taken -> Nothing
  | otherwise = Just $ insertAt (Just player) currentBoard position  -- Inserts Player at cell


-- replaces element of list at an index
-- if its out of range, does nothing
insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt item (_:xs) 0  = item:xs
insertAt item (x:xs) position = x:insertAt item xs (position -1)


-- Defines lines of board to check for wins at
checkLines :: [[Int]]
checkLines = [ [0,1,2], [3,4,5], [6,7,8]
             , [0,3,6], [1,4,7], [2,5,8]
             , [0,4,8], [2,4,6]]

-- checks board for a win on predefined lines
checkWin :: Board -> Maybe Player
checkWin currentboard = aux currentboard checkLines
  where
    aux :: Board -> [[Int]] -> Maybe Player
    aux _ [] = Nothing      -- base case, empty lines list, return Nothing, ie no win found
    aux input (indices:xs)
      | xWin input indices = Just X   -- checks for x win on current line
      | oWin input indices = Just O   -- checks for o win on current line
      | otherwise = aux currentboard xs  -- no win found, recurse starting at next line

-- checks board for draw
isDraw :: Board -> Bool
isDraw currentboard =
  all isJust currentboard &&  -- checks that current board is filled
 isNothing (checkWin currentboard)  -- checks no one has won



-- checks a list of cell positions for all Xs or Os
xWin :: Board -> [Int] -> Bool
xWin currentboard  = all $ (== Just X).(currentboard !!)

oWin :: Board -> [Int] -> Bool
oWin currentboard  = all $ (== Just O).(currentboard !!)


-- test board for debugging, this one is a draw
testBoardDraw :: Board
testBoardDraw = [ Just X, Just O, Just X
                  , Just X, Just O, Just O
                  , Just O, Just X, Just X]

-- test board for debugging, this one is a win
testBoardxwin :: Board
testBoardxwin = [ Just X, Nothing, Just O
                  , Just X, Just X, Just X
                  , Just O, Nothing, Nothing]

-- use to change player, swaps x to o and vice versa
switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

-- game entry point, using gloss play function
main :: IO ()
main = play
  (InWindow "Tic Tac Toe" (400, 400) (100, 100))  -- determines window size
  black                                           -- background window color
  10                                              -- frame rate
  initialState                                    -- initial state goes here
  drawGame                                        -- defines how to draw game based on state
  handleEvent                                     -- defines how to handle user events to modify state
  (\_ s -> s)                                     -- defines state modification based on time passing, not used here.
