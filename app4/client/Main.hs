{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Network.HTTP.Simple
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  let request = setRequestMethod "POST"
              $ setRequestPath "/test"
              $ setRequestHost "localhost"
              $ setRequestPort 3000
              $ setRequestSecure False
              $ setRequestBodyJSON (object ["test" .= True])
              $ defaultRequest

  response <- httpLBS request
  putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
  BL.putStrLn $ getResponseBody response
