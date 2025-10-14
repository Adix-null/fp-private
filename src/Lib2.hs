{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib2
  ( parseCommand,
    ToCliCommand (..),
    process,
  )
where

import qualified Lib1

type ErrorMsg = String

type Parser a = String -> Either ErrorMsg (a, String)

-- Simple parser for commands
parseCommand :: Parser Lib1.Command
parseCommand input =
  case words input of
    ["Dump", "Examples"] -> Right (Lib1.Dump Lib1.Examples, "")
    ["PrintFile"] -> Right (Lib1.PrintFile, "")
    ["EditFile", name] -> Right (Lib1.EditFile name, "")
    _ -> Left "Invalid command"

-- Default file for demo purposes
defaultFile :: Lib1.File
defaultFile = Lib1.File "default" "txt"

-- Process command (pure, no state stored between calls)
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) =
  "Examples:" : map toCliCommand Lib1.examples
process Lib1.PrintFile =
  [Lib1.name defaultFile ++ "." ++ Lib1.ext defaultFile]
process (Lib1.EditFile newName) =
  let edited = defaultFile {Lib1.name = newName}
   in ["File renamed (not persisted): " ++ Lib1.name edited ++ "." ++ Lib1.ext edited]
process c =
  ["Parsed as " ++ show c]

-- Class for converting commands to strings
class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.Dump Lib1.Examples) = "Dump Examples"
  toCliCommand Lib1.PrintFile = "PrintFile"
  toCliCommand (Lib1.EditFile name) = "EditFile " ++ name

-- Manual equality
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.Dump Lib1.Examples) == (Lib1.Dump Lib1.Examples) = True
  Lib1.PrintFile == Lib1.PrintFile = True
  (Lib1.EditFile a) == (Lib1.EditFile b) = a == b
  _ == _ = False
