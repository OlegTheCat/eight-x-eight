module InputProcessor where

import Game
import Input
import Prelude hiding (Left, Right)

processCommand :: InputCommand -> Game -> Game
processCommand CursorUp    = moveCursorInGame Up
processCommand CursorDown  = moveCursorInGame Down
processCommand CursorLeft  = moveCursorInGame Left
processCommand CursorRight = moveCursorInGame Right
processCommand Enter       = takeCellValue
