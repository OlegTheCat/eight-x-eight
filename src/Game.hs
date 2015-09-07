module Game (createGame) where
import qualified Data.Matrix as Matrix
import qualified Data.Traversable as Traversable
import           Prelude hiding (Left, Right)
import qualified System.Random as Random

boardWidth = 8
boardHeight = 8
cellMinValue = 1
cellMaxValue = 8


data Player = Player1 | Player2 deriving (Eq, Show)

data Cell = Cell Int | None deriving (Eq, Show)
type GameBoard = Matrix.Matrix Cell

type CursorPosition = (Int, Int)
data CursorMove = Left | Right | Up | Down deriving (Eq, Show)

data Game = Game { board :: GameBoard
                 , currentPlayer :: Player
                 , cursorPosition :: CursorPosition
                 , player1Score :: Int
                 , player2Score :: Int
                 } deriving (Eq, Show)

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

moveCursor :: CursorMove -> CursorPosition -> CursorPosition
moveCursor Left (x, y) = (checkWidth x (negate 1), y)
moveCursor Right (x, y) = (checkWidth x 1, y)
moveCursor Up (x, y) = (x, checkHeight y (negate 1))
moveCursor Down (x, y) = (x, checkHeight y 1)

moveCursorInGame :: Game -> CursorMove -> Game
moveCursorInGame game move = game { cursorPosition = moveCursor move (cursorPosition game) }


randomCell ::  IO Cell
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
