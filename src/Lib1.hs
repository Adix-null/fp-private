module Lib1
  ( examples,
    Command (..),
    Dumpable (..),
  )
where

import GHC.Generics (D)

data Dumpable = Examples
  deriving (Show)

-- The basic structure
data FS
  = File String String String -- name, ext, data
  | Folder String FS -- name, data rec
  deriving (Show, Eq)

data Command
  = Dump Dumpable
  | AddFile [String] String String String -- path name ext data
  | DeleteFile [String] String String -- path name ext
  | AddFolder [String] String -- path name
  | DeleteFolder [String] -- path
  | MoveFolder [String] [String] -- path_from path_to
  | MoveFile [String] [String] String String -- path_from path_to name ext
  | PrintFS -- print the whole filesystem
  deriving (Show)

apply :: Command -> FS -> FS
apply (Dump _) fs = fs
apply (AddFile path name ext content) fs = insertFile path name ext content fs
apply (DeleteFile path name ext) fs = deleteFile path name ext fs
apply (AddFolder path name) fs = insertFolder path name fs
apply (DeleteFolder path) fs = deleteFolder path fs
apply (MoveFile pathFrom pathTo name ext) fs = moveFile pathFrom pathTo name ext fs
apply (MoveFolder pathFrom pathTo) fs = moveFolder pathFrom pathTo fs
apply PrintFS fs = printFS fs

-- blank for now
insertFile :: [String] -> String -> String -> String -> FS -> FS
insertFile _ _ _ _ fs = fs

deleteFile :: [String] -> String -> String -> FS -> FS
deleteFile _ _ _ fs = fs

insertFolder :: [String] -> String -> FS -> FS
insertFolder _ _ fs = fs

deleteFolder :: [String] -> FS -> FS
deleteFolder _ fs = fs

moveFile :: [String] -> [String] -> String -> String -> FS -> FS
moveFile _ _ _ _ fs = fs

moveFolder :: [String] -> [String] -> FS -> FS
moveFolder _ _ fs = fs

printFS :: FS -> FS
printFS fs = fs

examples :: [Command]
examples =
  [ Dump Examples,
    AddFile ["r/f"] "file1" "txt" "Data of file",
    DeleteFile ["r/f"] "file1" "txt",
    AddFolder ["r/f"] "folder1",
    DeleteFolder ["r/f", "folder1"],
    MoveFile ["r/f"] ["folder1"] "file1" "txt",
    MoveFolder ["r/f", "folder1"] ["r/f"],
    PrintFS
  ]
