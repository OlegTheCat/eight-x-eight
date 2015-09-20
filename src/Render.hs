module Render where

import Game
import System.Console.ANSI (setCursorPosition,
                            setCursorColumn,
                            cursorForward,
                            clearScreen)

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
