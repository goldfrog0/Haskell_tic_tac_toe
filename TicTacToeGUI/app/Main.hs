module Main where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


data Player = X | O
  deriving (Eq, Show)

type Cell = Maybe Player

type Board = [Cell]

data GameState = GameState
  { board         :: Board
  , currentPlayer :: Player
  , gameOver      :: Maybe (Maybe Player)}

testGameState :: GameState
testGameState = GameState
  { board         = testBoardDraw
  , currentPlayer = X
  , gameOver      = Nothing}

initialState :: GameState
initialState = GameState
  { board         = emptyBoard
  , currentPlayer = X
  , gameOver      = Nothing}

drawGame :: GameState -> Picture
drawGame state = pictures [drawGrid, drawMarks (board state), drawGameOverMessage (gameOver state)]

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

intToGrid :: Int -> (Float, Float)
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

drawMark :: Int -> Maybe Player -> Picture
drawMark cell (Just X) = drawX $ intToGrid cell
drawMark cell (Just O) = drawO $ intToGrid cell
drawMark _    Nothing  = Blank

xPicture :: Picture
xPicture = pictures [diagonal1, diagonal2]
  where
    diagonal1 = line [(-60, -60),(60, 60)]
    diagonal2 = line [(-60, 60),(60, -60)]

drawX :: (Float, Float) -> Picture
drawX (a, b) = translate a b $ color red xPicture

drawO :: (Float, Float) -> Picture
drawO (a, b) = translate a b (color blue (thickCircle 60 5))


drawMarks :: Board -> Picture
drawMarks currentboard = pictures (zipWith drawMark [0..] currentboard)

drawGameOverMessage :: Maybe (Maybe Player) -> Picture
drawGameOverMessage Nothing = blank
drawGameOverMessage (Just Nothing)  = placeText "The game is a Draw, WOMP WOMP!!!"
drawGameOverMessage (Just (Just X)) = placeText "Congratulations, X wins"
drawGameOverMessage (Just (Just O)) = placeText "Congratulations, O wins"

placeText :: String -> Picture
placeText str = scale 0.3 0.3 $ translate (-700) 1050 $ color white $ text str

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Up _ (x, y)) state -- Catches mouse left click
  | isNothing $ gameOver state = tryPlacingMarkAt x y state  -- if the game is a not a draw or win, try player move
  | otherwise = state       --Reduntant default state
handleEvent _ state = state --Ignores any other input

tryPlacingMarkAt :: Float -> Float -> GameState -> GameState
tryPlacingMarkAt x y state =
  case positionToCell x y of
    Nothing -> state
    (Just idx) -> case makeMove (board state) idx (currentPlayer state) of
      Nothing -> state
      Just newBoard ->
        let newGameOver = case checkWin newBoard of
              Just winner -> Just (Just winner)
              Nothing     -> if isDraw newBoard
                             then Just Nothing
                             else Nothing
        in state { board = newBoard
                 , currentPlayer = switchPlayer (currentPlayer state)
                 , gameOver = newGameOver }

positionToCell :: Float -> Float -> Maybe Int
positionToCell x y
  | abs x > 300 || abs y > 300 = Nothing
  | otherwise = case (col, row) of
      (Just c, Just r) -> Just (r * 3 + c)
      _                -> Nothing
  where
    col
      | x < -100  = Just 0
      | x <  100  = Just 1
      | x <= 300  = Just 2
      | otherwise = Nothing

    row
      | y >   100 = Just 0
      | y >  -100 = Just 1
      | y >= -300 = Just 2
      | otherwise = Nothing

emptyBoard :: Board
emptyBoard = replicate 9 Nothing

makeMove :: Board -> Int -> Player -> Maybe Board
makeMove currentBoard position player
  | position < 0 || position > 8 = Nothing
  | isJust $ currentBoard !! position = Nothing
  | otherwise = Just $ insertAt (Just player) currentBoard position

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt item (_:xs) 0  = item:xs
insertAt item (x:xs) position = x:insertAt item xs (position -1)

checkLines :: [[Int]]
checkLines = [ [0,1,2], [3,4,5], [6,7,8]
             , [0,3,6], [1,4,7], [2,5,8]
             , [0,4,8], [2,4,6]]

checkWin :: Board -> Maybe Player
checkWin currentboard = aux currentboard checkLines
  where
    aux :: Board -> [[Int]] -> Maybe Player
    aux _ [] = Nothing
    aux input (indices:xs)
      | xWin input indices = Just X
      | oWin input indices = Just O
      | otherwise       = aux currentboard xs

isDraw :: Board -> Bool
isDraw currentboard = all isJust currentboard &&
  case checkWin currentboard of
                 Just _  -> False
                 Nothing -> True

xWin :: Board -> [Int] -> Bool
xWin currentboard indices = all ((== Just X) . (currentboard !!)) indices
oWin :: Board -> [Int] -> Bool
oWin currentboard indices = all ((== Just O) . (currentboard !!)) indices

testBoardDraw :: Board
testBoardDraw = [ Just X, Just O, Just X
                  , Just X, Just O, Just O
                  , Just O, Just X, Just X]

testBoardxwin :: Board
testBoardxwin = [ Just X, Nothing, Just O
                  , Just X, Just X, Just X
                  , Just O, Nothing, Nothing]

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

main :: IO ()
main = play
  (InWindow "Tic Tac Toe" (400, 400) (100, 100))
  black
  10
  initialState
  drawGame
  handleEvent
  (\_ s -> s)
