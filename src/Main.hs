module Main(main) where

import Data.Char (ord)
import Game
import Input
import InputProcessor
import Render
import System.Console.ANSI (clearScreen)
import System.IO

gameLoop :: Game -> IO ()
gameLoop g =
  do
    renderGame g
    if isGameFinished g
      then return ()
      else do
      cmd <- getInputCommand
      gameLoop $ processCommand cmd g

main =
  do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    g <- createGame
    gameLoop g
    -- renderGame pureGame
    -- putStrLn "========================="
    -- print $ show pureGame
    -- g <- createGame
    -- renderGame g
