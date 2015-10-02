module Render where

import Game
import System.Console.ANSI (setCursorPosition,
                            setCursorColumn,
                            cursorForward,
                            clearScreen)
import System.IO


cellWidth = 5
cellHeight = 5

cellCoordsToTerminalCoords :: Coords -> Coords
cellCoordsToTerminalCoords (x, y) = (x * (cellWidth-1), y * (cellHeight-1))

renderGame :: Game -> IO ()
renderGame g =
  do
    clearScreen
    renderGameBoard $ board g
    renderCursorAtCoords $ cursorPosition g
    renderGameStatsAtTerminalCoords (40, 5) g
    return ()

renderCursorAtTerminalCoords :: Coords -> IO ()
renderCursorAtTerminalCoords (x, y) =
    do
    setCursorPosition y x
    putStrLn "*****"
    setCursorColumn x
    putStrLn "*   *"
    setCursorColumn x
    putStr "* "
    cursorForward 1
    putStr " *"
    putStrLn ""
    setCursorColumn x
    putStrLn "*   *"
    setCursorColumn x
    putStrLn "*****"

renderCursorAtCoords :: Coords -> IO ()
renderCursorAtCoords = renderCursorAtTerminalCoords . cellCoordsToTerminalCoords

renderCellAtTerminalCoordsWithContent :: String -> Coords -> IO ()
renderCellAtTerminalCoordsWithContent s (x, y) =
  do
    setCursorPosition y x
    putStrLn "+---+"
    setCursorColumn x
    putStrLn "|   |"
    setCursorColumn x
    putStr "| "
    putStr s
    putStr " |"
    putStrLn ""
    setCursorColumn x
    putStrLn "|   |"
    setCursorColumn x
    putStrLn "+---+"

renderCellAtTerminalCoords :: Cell -> Coords -> IO ()
renderCellAtTerminalCoords (Cell x) = renderCellAtTerminalCoordsWithContent $ show x
renderCellAtTerminalCoords None     = renderCellAtTerminalCoordsWithContent " "

renderCellAtCoords :: Cell -> Coords -> IO ()
renderCellAtCoords cell coords = renderCellAtTerminalCoords cell $ cellCoordsToTerminalCoords coords

renderGameBoard :: GameBoard -> IO [()]
renderGameBoard board =
  mapM
  (\coords -> renderCellAtCoords (getCell board coords) coords)
  $ boardCoordsRange board

renderGameStatsAtTerminalCoords :: Coords -> Game -> IO ()
renderGameStatsAtTerminalCoords coords@(x, y) g =
  do
    renderCurrentPlayerAtTerminalCoords coords (currentPlayer g)
    renderPlayerScoreAtTerminalCoords (x, y + 1) Player1 (player1Score g)
    renderPlayerScoreAtTerminalCoords (x, y + 2) Player2 (player2Score g)

renderCurrentPlayerAtTerminalCoords :: Coords -> Player -> IO ()
renderCurrentPlayerAtTerminalCoords coords Player1 =
  renderTextAtTerminalCoords coords "Current Player: Player #1"
renderCurrentPlayerAtTerminalCoords coords Player2 =
  renderTextAtTerminalCoords coords "Current Player: Player #2"


renderPlayerScoreAtTerminalCoords :: Coords -> Player -> Int -> IO ()
renderPlayerScoreAtTerminalCoords coords Player1 s =
  renderScoreWithLabelAtTerminalCoords coords "Player #1: " s
renderPlayerScoreAtTerminalCoords coords Player2 s =
  renderScoreWithLabelAtTerminalCoords coords "Player #2: " s

renderScoreWithLabelAtTerminalCoords :: Coords -> String -> Int -> IO ()
renderScoreWithLabelAtTerminalCoords coords label s =
  renderTextAtTerminalCoords coords (label ++ show s)

renderTextAtTerminalCoords :: Coords -> String -> IO ()
renderTextAtTerminalCoords (x, y) s =
  do
    setCursorPosition y x
    putStr s
    hFlush stdout
