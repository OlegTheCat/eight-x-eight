module InputProcessor where

import Game
import Input
import Prelude hiding (Left, Right)

processCommand :: InputCommand -> Game -> Game
processCommand CursorUp g    = moveCursorInGame g Up
processCommand CursorDown g  = moveCursorInGame g Down
processCommand CursorLeft g  = moveCursorInGame g Left
processCommand CursorRight g = moveCursorInGame g Right
