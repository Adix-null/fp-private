{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Main where

import Control.Exception (SomeException)
import Data.Aeson
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


stateFileName :: String
stateFileName = "fs_state.txt"

trim :: String -> String
trim = T.unpack . T.strip . T.pack

main :: IO ()
main = do
  st <- loadState stateFileName
  scotty 3000 $ do
    post "/cmd" $ do
      body <- jsonData `rescue` (\(_ :: SomeException) -> return $ Object mempty)
      case body of
        Object obj -> case parseMaybe (.: fromString "cmd") obj of
          Just cmdStr -> do
            -- parse the command string
            case runParser parseCommand (trim cmdStr) of
              Left err -> do
                  status status404
                  text (TL.pack $ "Parse error: " ++ err)
              Right cmd -> do
                  -- update FSState
                  _ <- liftIO $ execStateT (runCommand cmd) st
                  liftIO $ saveState stateFileName st
                  status status200 
                  text (TL.pack "OK")
          Nothing -> do
            status status404
            text "Missing 'cmd'"
        _ -> do
          status status404
          text "Invalid JSON"