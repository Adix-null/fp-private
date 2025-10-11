{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib2
  ( parseCommand,
    ToCliCommand (..),
    process,
  )
where

import Data.List (stripPrefix)
import qualified Lib1

type ErrorMsg = String

type Parser a = String -> Either ErrorMsg (a, String)

-- | Parses user's input.
parseCommand :: Parser Lib1.Command
parseCommand input
  | Just rest <- stripPrefix "AddFolder " input,
    (path, ' ' : name) <- break (== ' ') rest =
      Right (Lib1.AddFolder path name, "") -- remaining string is empty
  | otherwise = Left "Command not recognized"

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ toCliCommand c]

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.AddFolder path name) = "AddFolder " ++ path ++ " " ++ name
  toCliCommand _ = "<other command>"

-- Eq instance (minimal for now)
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.AddFolder p1 n1) == (Lib1.AddFolder p2 n2) = p1 == p2 && n1 == n2
  _ == _ = False
