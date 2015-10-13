module Game where

import           Data.Ix (range)
import qualified Data.Matrix as Matrix
import qualified Data.Traversable as Traversable
import           Data.Vector (any)
import           Prelude hiding (Left, Right, any)
import qualified System.Random as Random

boardWidth = 8
boardHeight = 8
cellMinValue = 1
cellMaxValue = 8


data Player = Player1 | Player2 deriving (Eq, Show)

data Cell = Cell Int | None deriving (Eq, Show)

isNoneCell :: Cell -> Bool
isNoneCell None = True
isNoneCell _    = False

type GameBoard = Matrix.Matrix Cell

type Coords = (Int, Int)
data CursorMove = Left | Right | Up | Down deriving (Eq, Show)

data Game = Game { board :: GameBoard
                 , currentPlayer :: Player
                 , cursorPosition :: Coords
                 , player1Score :: Int
                 , player2Score :: Int
                 } deriving (Eq, Show)

getCell :: GameBoard -> Coords -> Cell
getCell board (x, y) = board Matrix.! (y + 1, x + 1)

setCell :: GameBoard -> Coords -> Cell -> GameBoard
setCell board (x, y) cell = Matrix.setElem cell (y + 1, x + 1) board

setNoneCell :: GameBoard -> Coords -> GameBoard
setNoneCell board coords = setCell board coords None

boardCoordsRange :: GameBoard -> [Coords]
boardCoordsRange m =
  [(x, y) | x <- [0 .. Matrix.ncols m - 1], y <- [0 .. Matrix.nrows m - 1]]

initialGame :: GameBoard -> Game
initialGame board =
  Game { board = board,
         currentPlayer = Player1,
         cursorPosition = (0, 0),
         player1Score = 0,
         player2Score = 0 }

checkRange :: Int -> Int -> Int -> Int -> Int
checkRange x dx lowerBound upperBound =
  if (x + dx) >= upperBound || (x + dx) < lowerBound
  then x
  else x + dx

checkWidth :: Int -> Int -> Int
checkWidth x dx = checkRange x dx 0 boardWidth

checkHeight :: Int -> Int -> Int
checkHeight x dx = checkRange x dx 0 boardHeight

moveCursor :: CursorMove -> Coords -> Coords
moveCursor Left  (x, y) = (checkWidth x (negate 1), y)
moveCursor Right (x, y) = (checkWidth x 1, y)
moveCursor Up    (x, y) = (x, checkHeight y (negate 1))
moveCursor Down  (x, y) = (x, checkHeight y 1)

moveCursorForPlayer :: Player -> CursorMove -> Coords -> Coords
moveCursorForPlayer Player1 Up    = moveCursor Up
moveCursorForPlayer Player1 Down  = moveCursor Down
moveCursorForPlayer Player1 _     = id
moveCursorForPlayer Player2 Left  = moveCursor Left
moveCursorForPlayer Player2 Right = moveCursor Right
moveCursorForPlayer Player2 _     = id

moveCursorInGame :: Game -> CursorMove -> Game
moveCursorInGame game move = game { cursorPosition = moveCursorForPlayer
                                                     (currentPlayer game)
                                                     move
                                                     (cursorPosition game) }

switchPlayer :: Game -> Game
switchPlayer g = doSwitch $ currentPlayer g
  where doSwitch Player1 = g { currentPlayer = Player2 }
        doSwitch Player2 = g { currentPlayer = Player1 }

incCurrentPlayerScore :: Game -> Int -> Game
incCurrentPlayerScore g =
  incPlayerScore g (currentPlayer g)

incPlayerScore :: Game -> Player -> Int  -> Game
incPlayerScore g Player1 x = g { player1Score = (player1Score g) + x }
incPlayerScore g Player2 x = g { player2Score = (player2Score g) + x }

getCellAtCursorPosition :: Game -> Cell
getCellAtCursorPosition g = getCell (board g) (cursorPosition g)

setCellAtCursorPosition :: Game -> Cell -> Game
setCellAtCursorPosition g c = g { board = setCell (board g) (cursorPosition g) c }

setNoneCellAtCursorPosition :: Game -> Game
setNoneCellAtCursorPosition g = setCellAtCursorPosition g None

takeCellValue :: Game -> Game
takeCellValue g  =
  case getCellAtCursorPosition g of
   Cell x -> (switchPlayer . (\g -> incCurrentPlayerScore g x) . setNoneCellAtCursorPosition) g
   None -> g

isGameFinished :: Game -> Bool
isGameFinished g = not $ playerHasAvailableMoves (currentPlayer g) g
  where
    playerHasAvailableMoves Player1 g =
      hasNonEmptyCells $ getCurrentCol g
    playerHasAvailableMoves Player2 g =
      hasNonEmptyCells $ getCurrentRow g
    hasNonEmptyCells v =
      any (not . isNoneCell) v
    getCurrentRow g =
      Matrix.getRow (succ $ snd $ cursorPosition g) (board g)
    getCurrentCol g =
      Matrix.getCol (succ $ fst $ cursorPosition g) (board g)


purePopulateBoard :: GameBoard
purePopulateBoard =
  Matrix.matrix
  boardHeight
  boardWidth
  (\_ -> Cell 5)

pureGame :: Game
pureGame =
  initialGame purePopulateBoard

randomCell :: IO Cell
randomCell = do
  i <- Random.getStdRandom $
       Random.randomR (cellMinValue, cellMaxValue)
  return $ Cell i

populateBoard :: IO GameBoard
populateBoard = Traversable.sequenceA $
                Matrix.matrix
                boardHeight
                boardWidth
                (\_ -> randomCell)

createGame :: IO Game
createGame = do
  board <- populateBoard
  return $ initialGame board
