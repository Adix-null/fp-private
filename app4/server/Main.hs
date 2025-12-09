{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Main where

import Control.Exception (SomeException)
import Data.Aeson ( Value(Object), (.:) )
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import FSLogic
import StateInterpreter
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Lib4 (parseCommand)
import Network.HTTP.Types.Status
import Web.Scotty
import Data.IORef

import Lib1

stateFileName :: String
stateFileName = "fs_state.txt"

trim :: String -> String
trim = T.unpack . T.strip . T.pack

main :: IO ()
main = do
  stRef <- newIORef =<< loadState stateFileName -- mutable FSState
  scotty 3000 $ do
    post "/cmd" $ do
      body <- jsonData `rescue` (\(_ :: SomeException) -> return $ Object mempty)
      case body of
        Object obj -> case parseMaybe (.: fromString "cmd") obj of
          Just cmdStr -> do
            case runParser parseCommand (trim cmdStr) of
              Left err -> do
                status status400
                text (TL.pack $ "Bad Request Parse error: " ++ err)
              Right cmd -> do
                st <- liftIO $ readIORef stRef
                newSt <- liftIO $ execStateT (runCommand cmd) st
                liftIO $ writeIORef stRef newSt
                liftIO $ saveState stateFileName newSt
                case cmd of
                  PrintFS -> do
                    setHeader "Content-Type" "text/plain"
                    text (TL.pack $ unlines $ formattedTreeOtp (buildTree (reverse (unFS newSt))))
                  _ -> do
                    status $ case cmd of
                      AddFile{} -> status201
                      AddFolder{} -> status201
                      AddFolderAtRoot{} -> status201
                      MoveFile{} -> status202
                      MoveFolder{} -> status202
                      DeleteFile{} -> status204
                      DeleteFolder{} -> status204
                      _ -> status200
                    text "Success"
          Nothing -> do
            status status404
            text "Not Found cmd"
        _ -> do
          status status400
          text "Bad Request Invalid JSON"