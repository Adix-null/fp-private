{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (parseMaybe)
import Data.Text.Lazy (pack)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.HTTP.Types.Status (badRequest400, notFound404, ok200, status200, status404)
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  post "/test" $ do
    body <- jsonData `rescue` (\(_ :: SomeException) -> return $ Object mempty)
    case body of
      Object obj -> case parseMaybe (.: fromString "test") obj of
        Just True -> status status200 >> text "OK"
        Just False -> status status404 >> text "Not found"
        _ -> status status404 >> text "Invalid"
      _ -> status status404 >> text "Invalid"