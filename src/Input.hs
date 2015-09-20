module Input where

import Game
import Data.Char(ord)

data InputCommand = CursorLeft | CursorRight | CursorUp | CursorDown | Enter deriving (Show, Eq)

charToCommand :: Char -> Maybe InputCommand
charToCommand 'w'   = Just CursorUp
charToCommand 's'   = Just CursorDown
charToCommand 'a'   = Just CursorLeft
charToCommand 'd'   = Just CursorRight
charToCommand '\10' = Just Enter
charToCommand _     = Nothing

getInputCommand :: IO InputCommand
getInputCommand =
  do
    c <- getChar
    maybe getInputCommand return $ charToCommand c
