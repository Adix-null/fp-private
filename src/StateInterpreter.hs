module StateInterpreter where

import Control.Monad.State.Strict
import FSLogic
import Lib1

runCommand :: Command -> StateT FSState IO ()
runCommand cmd = case cmd of
  PrintFS -> do
    FSState cmds <- get
    let tree = buildTree (reverse cmds)
    liftIO $ mapM_ putStrLn (formattedTreeOtp tree)
  _ -> do
    st <- get
    put (computeNextState st cmd)
