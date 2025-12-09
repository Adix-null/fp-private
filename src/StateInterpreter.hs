{-# LANGUAGE OverloadedStrings #-}

module StateInterpreter where

import Control.Monad.State.Strict
import FSLogic
import Lib1
import Data.Aeson (object, Value, (.=))

runCommand :: Command -> StateT FSState IO Data.Aeson.Value
runCommand cmd = case cmd of
  PrintFS -> do
    FSState cmds <- get
    let tree = buildTree (reverse cmds)
        output = unlines $ formattedTreeOtp tree
    return $ Data.Aeson.object ["output" Data.Aeson..= output]
  _ -> do
    st <- get
    put (computeNextState st cmd)
    return $ Data.Aeson.object ["status" Data.Aeson..= ("ok" :: String)]