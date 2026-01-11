{-# LANGUAGE OverloadedStrings #-}

module Main where

import DSL
import Data.Aeson (Value, object, (.=), decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import FSLogic
import Lib1
import Network.HTTP.Simple

sendCommand :: Command -> IO Value
sendCommand cmd = do
  let req =
        setRequestMethod "POST" $
          setRequestHost "localhost" $
            setRequestPort 3000 $
              setRequestPath "/cmd" $
                setRequestSecure False $
                  setRequestBodyJSON (object ["cmd" .= ("AddFolder root f1" :: String)]) 
                    defaultRequest
  resp <- httpLBS req
  return $ maybe (object []) id $ decode (getResponseBody resp)


-- Wrap a program in FSFree and run via HTTP
runProgram :: FSFree a -> IO a
runProgram = runWeb sendCommand

main :: IO ()
main = do
  runProgram $ do
    addFolderAtRoot $ Lib1.stringToAlphanumStr "root"
    let myFile = Lib1.File (Lib1.Name (Lib1.Rec (Lib1.Lower 'f') (Lib1.Single (Lib1.Lower '1'))) Lib1.Txt)
                            (Lib1.SingleASCII (Lib1.Alphanum (Lib1.Lower 'a')))
    addFile (SinglePath $ Lib1.stringToAlphanumStr "root") myFile
    _ <- printFS
    return ()
  putStrLn "Done"
