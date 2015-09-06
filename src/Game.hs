module Game where
import qualified Data.Matrix as Matrix
import qualified Data.Traversable as Traversable
import qualified System.Random as Random

boardWidth = 8
boardHeight = 8
cellMinValue = 1
cellMaxValue = 8

data Cell = Cell Int | None
data Player = Player1 | Player2

type GameBoard = Matrix.Matrix Cell
type CursorPosition = (Int, Int)

data Game = Game { board :: GameBoard
                 , currentPlayer :: Player
                 , cursorPosition :: CursorPosition
                 , player1Score :: Int
                 , player2Score :: Int
                 }

randomCell :: IO Cell
randomCell = do
  gen <- Random.getStdGen
  let (i, _) = Random.randomR (cellMinValue, cellMaxValue) gen in
   return $ Cell i

populateBoard :: IO GameBoard
populateBoard = Traversable.sequenceA $
                Matrix.matrix
                boardHeight
                boardWidth
                (\_ -> randomCell)

initialGame :: GameBoard -> Game
initialGame board =
  Game { board = board,
         currentPlayer = Player1,
         cursorPosition = (0, 0),
         player1Score = 0,
         player2Score = 0 }

createGame :: IO Game
createGame = do
  board <- populateBoard
  return $ initialGame board
