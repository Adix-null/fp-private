{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module DSL where

import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict
import Data.Aeson (Value)
import FSLogic
import Lib1

-- | Algebra for filesystem commands
data FSAlgebra next
  = FSAddFile Path File (() -> next)
  | FSDeleteFile Path Name (() -> next)
  | FSAddFolder Path AlphanumStr (() -> next)
  | FSAddFolderAtRoot AlphanumStr (() -> next)
  | FSMoveFile Path Path Name (() -> next)
  | FSMoveFolder Path Path (() -> next)
  | FSDeleteFolder Path (() -> next)
  | FSPrintFS (Value -> next)
  | FSOther Command (() -> next)
  deriving (Functor)

-- | Free monad type
type FSFree = Free FSAlgebra

-- | Smart constructors
addFile :: Path -> File -> FSFree ()
addFile p f = Free $ FSAddFile p f Pure

deleteFile :: Path -> Name -> FSFree ()
deleteFile p n = Free $ FSDeleteFile p n Pure

addFolder :: Path -> AlphanumStr -> FSFree ()
addFolder p name = Free $ FSAddFolder p name Pure

addFolderAtRoot :: AlphanumStr -> FSFree ()
addFolderAtRoot name = Free $ FSAddFolderAtRoot name Pure

moveFile :: Path -> Path -> Name -> FSFree ()
moveFile from to n = Free $ FSMoveFile from to n Pure

moveFolder :: Path -> Path -> FSFree ()
moveFolder from to = Free $ FSMoveFolder from to Pure

deleteFolder :: Path -> FSFree ()
deleteFolder p = Free $ FSDeleteFolder p Pure

printFS :: FSFree Value
printFS = Free $ FSPrintFS Pure

other :: Command -> FSFree ()
other c = Free $ FSOther c Pure

-- | In-memory interpreter
runInMemory :: FSFree a -> StateT FSState IO a
runInMemory (Pure v) = return v
runInMemory (Free step) = case step of
  FSAddFile p f next -> modify (`computeNextState` AddFile p f) >> runInMemory (next ())
  FSDeleteFile p n next -> modify (`computeNextState` DeleteFile p n) >> runInMemory (next ())
  FSAddFolder p name next -> modify (`computeNextState` AddFolder p name) >> runInMemory (next ())
  FSAddFolderAtRoot name next -> modify (`computeNextState` AddFolderAtRoot name) >> runInMemory (next ())
  FSMoveFile from to n next -> modify (`computeNextState` MoveFile from to n) >> runInMemory (next ())
  FSMoveFolder from to next -> modify (`computeNextState` MoveFolder from to) >> runInMemory (next ())
  FSDeleteFolder p next -> modify (`computeNextState` DeleteFolder p) >> runInMemory (next ())
  FSPrintFS next -> do
    st <- get
    val <- liftIO $ evalStateT (runCommand PrintFS) st
    runInMemory (next val)
  FSOther c next -> modify (`computeNextState` c) >> runInMemory (next ())

-- | Web / IO interpreter (uses your HTTP client)
runWeb :: (Command -> IO Value) -> FSFree a -> IO a
runWeb sendCommand (Pure v) = return v
runWeb sendCommand (Free step) = case step of
  FSAddFile p f next -> sendCommand (AddFile p f) >> runWeb sendCommand (next ())
  FSDeleteFile p n next -> sendCommand (DeleteFile p n) >> runWeb sendCommand (next ())
  FSAddFolder p name next -> sendCommand (AddFolder p name) >> runWeb sendCommand (next ())
  FSAddFolderAtRoot name next -> sendCommand (AddFolderAtRoot name) >> runWeb sendCommand (next ())
  FSMoveFile from to n next -> sendCommand (MoveFile from to n) >> runWeb sendCommand (next ())
  FSMoveFolder from to next -> sendCommand (MoveFolder from to) >> runWeb sendCommand (next ())
  FSDeleteFolder p next -> sendCommand (DeleteFolder p) >> runWeb sendCommand (next ())
  FSPrintFS next -> do
    val <- sendCommand PrintFS
    runWeb sendCommand (next val)
  FSOther c next -> sendCommand c >> runWeb sendCommand (next ())