module Lib1
  ( examples,
    Command (..),
    Dumpable (..),
    File (..),
  )
where

data Dumpable = Examples
  deriving (Show)

-- Represents a file (name + extension)
data File = File
  { name :: String,
    ext :: String
  }
  deriving (Show)

-- Available commands
data Command
  = Dump Dumpable
  | PrintFile
  | EditFile String
  deriving (Show)

-- Example commands
examples :: [Command]
examples =
  [ Dump Examples,
    PrintFile,
    EditFile "abcd"
  ]
