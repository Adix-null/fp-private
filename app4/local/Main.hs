module Main where

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import FSLogic
import Lib4
import StateInterpreter

main :: IO ()
main = do
  st <- loadState "fs_state.txt"
  final <- execStateT loop st
  saveState "fs_state.txt" final

loop :: StateT FSState IO ()
loop = do
  line <- liftIO getLine
  case runParser parseCommand line of
    Left err -> do
      liftIO (putStrLn ("Parse error: " ++ err))
      loop
    Right cmd -> do
      runCommand cmd
      loop
