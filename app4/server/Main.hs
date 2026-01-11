{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import DSL
import Data.IORef
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty as S
import FSLogic
import Lib1
import Web.Scotty

stateFileName :: String
stateFileName = "fs_state.txt"

-- Example program: add folder + a file
exampleProgram :: FSFree ()
exampleProgram = do
  addFolderAtRoot $ stringToAlphanumStr "example"
  let myFile =
        File
          (Name (Rec (Lower 'f') (Single (Lower '1'))) Txt)
          (SingleASCII (Alphanum (Lower 'x')))
  addFile (SinglePath $ stringToAlphanumStr "example") myFile
  _ <- printFS
  return ()

main :: IO ()
main = do
  stRef <- newIORef =<< loadState stateFileName
  scotty 3000 $ do
    S.get "/example" $ do
      st <- liftIO $ readIORef stRef
      newSt <- liftIO $ execStateT (runInMemory exampleProgram) st
      liftIO $ writeIORef stRef newSt
      liftIO $ saveState stateFileName newSt
      text "Ran example program successfully!"
