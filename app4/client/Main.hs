{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.HTTP.Simple
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  let request = setRequestMethod "POST"
              $ setRequestPath "/cmd"
              $ setRequestHost "localhost"
              $ setRequestPort 3000
              $ setRequestSecure False
              $ setRequestBodyJSON (object ["cmd" .= ("AddFile f1/f2 mp.exe#123$%" :: String)])
              $ defaultRequest

  response <- httpLBS request
  putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
  BL.putStrLn $ getResponseBody response
