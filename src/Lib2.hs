{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib2
  ( parseCommand,
    ToCliCommand (..),
    process,
  )
where

import Data.Char (isAlpha, isDigit, toLower, toUpper)
import Data.Foldable
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
import Data.Monoid
import qualified Lib1

type ErrorMsg = String

type Parser e a = String -> Either e (a, String)

-- lesson helper utility stuff
parseLetter :: Parser ErrorMsg Char
parseLetter [] = Left "A letter is expected but got empty input"
parseLetter (h : t) =
  if isAlpha h
    then Right (h, t)
    else Left $ "A letter is expected, but got " ++ [h]

parseDigit :: Parser ErrorMsg Char
parseDigit [] = Left "A digit is expected but got empty input"
parseDigit (h : t) =
  if isDigit h
    then Right (h, t)
    else Left $ "A digit is expected, but got " ++ [h]

parseAlphaNum :: Parser ErrorMsg Char
parseAlphaNum input =
  case parseLetter input of
    Right r -> Right r
    Left _ -> parseDigit input

many :: Parser e a -> Parser e [a]
many p = many' p []
  where
    many' p' acc input =
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

many1 :: Parser ErrorMsg a -> Parser ErrorMsg [a]
many1 p input =
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left "At least on value required"
    Right a -> Right a

pmap :: (a -> b) -> Parser e a -> Parser e b
pmap f p input =
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)

keyword :: String -> Parser ErrorMsg String
keyword prefix input
  | prefix `isPrefixOf` input = Right (prefix, drop (length prefix) input)
  | otherwise = Left $ prefix ++ " is expected, got " ++ input

orElse :: (Semigroup e) => Parser e a -> Parser e a -> Parser e a
orElse p1 p2 input =
  case p1 input of
    Right r1 -> Right r1
    Left e1 ->
      case p2 input of
        Right r2 -> Right r2
        Left e2 -> Left $ e1 <> e2

and3 :: Parser e a -> Parser e b -> Parser e c -> Parser e (a, b, c)
and3 p1 p2 p3 input =
  case p1 input of
    Left e1 -> Left e1
    Right (v1, r1) ->
      case p2 r1 of
        Left e2 -> Left e2
        Right (v2, r2) ->
          case p3 r2 of
            Left e3 -> Left e3
            Right (v3, r3) -> Right ((v1, v2, v3), r3)

ws :: Parser ErrorMsg String
ws = pmap concat (many1 (keyword " " `orElse` keyword "\t"))

parseAlphaNumStr :: Parser ErrorMsg String
parseAlphaNumStr = many1 parseAlphaNum

parseOneOf :: [String] -> Parser ErrorMsg String
parseOneOf exts = foldr1 orElse (map keyword exts)

parseExt :: Parser ErrorMsg String
parseExt = parseOneOf ["txt", "png", "jpg", "json", "dat", "exe", "hs", "cs", "html", "cpp", "mp4", "mp3"]

parseFilename :: Parser ErrorMsg String
parseFilename =
  pmap (\(name, _dot, ext) -> name ++ "." ++ ext) $
    and3 parseAlphaNumStr (keyword ".") parseExt


parseAddFile :: Parser Lib1.Command
parseAddFile =

-- parseMoveFile :: Parser Lib1.Command
-- parseAddFile =

-- parseDeleteFile :: Parser Lib1.Command
-- parseDeleteFile =

-- parseAddFolder :: Parser Lib1.Command
-- parseAddFolder =

-- parseMoveFolder :: Parser Lib1.Command
-- parseMoveFolder =

-- parseDeleteFolder :: Parser Lib1.Command 
-- parseDeleteFolder = 

-- | Parses user's input.
-- The function must be implemented and must have tests.
parseCommand :: Parser Lib1.Command
parseCommand =
    parseAddFile
    -- `orElse` parseMoveFile
    -- `orElse` parseDeleteFile
    -- `orElse` parseAddFolder
    -- `orElse` parseMoveFolder
    -- `orElse` parseDeleteFolder
parseCommand _ = Left "Not implemented"

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "EXAMPLES:" : map toCliCommand Lib1.examples
process (Lib1.AddFile (Lib1.AddFileC path file)) = "Example AddFile: " : [toCliCommand (Lib1.AddFile (Lib1.AddFileC path file))]
process (Lib1.MoveFile (Lib1.MoveFileC from to fname)) = "Example MoveFile: " : [toCliCommand (Lib1.MoveFile (Lib1.MoveFileC from to fname))]
process (Lib1.DeleteFile (Lib1.DeleteFileC path fname)) = "Example DeleteFile: " : [toCliCommand (Lib1.DeleteFile (Lib1.DeleteFileC path fname))]
process (Lib1.AddFolder (Lib1.AddFolderC path folderName)) = "Example AddFolder: " : [toCliCommand (Lib1.AddFolder (Lib1.AddFolderC path folderName))]
process (Lib1.MoveFolder (Lib1.MoveFolderC from to)) = "Example MoveFolder: " : [toCliCommand (Lib1.MoveFolder (Lib1.MoveFolderC from to))]
process (Lib1.DeleteFolder (Lib1.DeleteFolderC path)) = "Example DeleteFolder: " : [toCliCommand (Lib1.DeleteFolder (Lib1.DeleteFolderC path))]
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

-- | You have to make your Command an instance of ToCliCommand class.
-- Please remove all custom Show instances of Command ADT and
-- use "deriving Show" only.
instance ToCliCommand Lib1.Command where
  toCliCommand (Lib1.AddFile (Lib1.AddFileC path file)) = "AddFile " ++ showPath path ++ " " ++ showFile file
  toCliCommand (Lib1.MoveFile (Lib1.MoveFileC from to fname)) = "MoveFile " ++ showPath from ++ " " ++ showPath to ++ " " ++ showName fname
  toCliCommand (Lib1.DeleteFile (Lib1.DeleteFileC path fname)) = "DeleteFile " ++ showPath path ++ " " ++ showName fname
  toCliCommand (Lib1.AddFolder (Lib1.AddFolderC path folderName)) = "AddFolder " ++ showPath path ++ " " ++ showFolderName folderName
  toCliCommand (Lib1.MoveFolder (Lib1.MoveFolderC from to)) = "MoveFolder " ++ showPath from ++ " " ++ showPath to
  toCliCommand (Lib1.DeleteFolder (Lib1.DeleteFolderC path)) = "DeleteFolder " ++ showPath path
  toCliCommand (Lib1.Dump d) = "Dump " ++ show d

-- Helper functions to show components
showFile :: Lib1.File -> String
showFile (Lib1.File name dat) = showName name ++ "#" ++ showData dat
  where
    showData :: Lib1.Data -> String
    showData (Lib1.SingleASCII ascii) = showASCII ascii
    showData (Lib1.RecASCII ascii rest) = showASCII ascii ++ showData rest

    showASCII :: Lib1.ASCII -> String
    showASCII (Lib1.ASCII az sym) = showAzAZ09 az ++ showSymbol sym

    showSymbol :: Lib1.Symbol -> String
    showSymbol sym = case sym of
      Lib1.SymTab -> "\t"
      Lib1.SymExclam -> "!"
      Lib1.SymQuote -> "\""
      Lib1.SymDollar -> "$"
      Lib1.SymPercent -> "%"
      Lib1.SymAmpersand -> "&"
      Lib1.SymApostrophe -> "'"
      Lib1.SymLParen -> "("
      Lib1.SymRParen -> ")"
      Lib1.SymAsterisk -> "*"
      Lib1.SymPlus -> "+"
      Lib1.SymComma -> ","
      Lib1.SymMinus -> "-"
      Lib1.SymDot -> "."
      Lib1.SymColon -> ":"
      Lib1.SymSemicolon -> ";"
      Lib1.SymLt -> "<"
      Lib1.SymEq -> "="
      Lib1.SymGt -> ">"
      Lib1.SymQMark -> "?"
      Lib1.SymAt -> "@"
      Lib1.SymBackslash -> "\\"
      Lib1.SymCaret -> "^"
      Lib1.SymUnderscore -> "_"
      Lib1.SymBacktick -> "`"
      Lib1.SymLCurly -> "{"
      Lib1.SymPipe -> "|"
      Lib1.SymRCurly -> "}"
      Lib1.SymTilde -> "~"

showFolderName :: Lib1.AlphanumStr -> String
showFolderName = showAlphanumStr

showPath :: Lib1.Path -> String
showPath (Lib1.SinglePath a) = showAlphanumStr a
showPath (Lib1.RecPath a rest) = showAlphanumStr a ++ "/" ++ showPath rest

showAlphanumStr :: Lib1.AlphanumStr -> String
showAlphanumStr (Lib1.Single c) = showAzAZ09 c
showAlphanumStr (Lib1.Rec c cs) = showAzAZ09 c ++ showAlphanumStr cs

showAzAZ09 :: Lib1.AzAZ09 -> String
showAzAZ09 (Lib1.Lower c) = [c]
showAzAZ09 (Lib1.Upper c) = [c]
showAzAZ09 (Lib1.Digit c) = [c]

showName :: Lib1.Name -> String
showName (Lib1.Name alphanum ext) = showAlphanumStr alphanum ++ "." ++ show ext

-- | You have to make your Command an instance of Eq class.
-- Usage of "deriving Eq" is forbidden.
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.AddFile (Lib1.AddFileC pf1 d1) == Lib1.AddFile (Lib1.AddFileC pf2 d2) = pf1 == pf2 && d1 == d2
  Lib1.MoveFile (Lib1.MoveFileC pf1 pt1 fln1) == Lib1.MoveFile (Lib1.MoveFileC pf2 pt2 fln2) = pf1 == pf2 && pt1 == pt2 && fln1 == fln2
  Lib1.DeleteFile (Lib1.DeleteFileC pf1 fln1) == Lib1.DeleteFile (Lib1.DeleteFileC pf2 fln2) = pf1 == pf2 && fln1 == fln2
  Lib1.AddFolder (Lib1.AddFolderC pf1 fdn1) == Lib1.AddFolder (Lib1.AddFolderC pf2 fdn2) = pf1 == pf2 && fdn1 == fdn2
  Lib1.MoveFolder (Lib1.MoveFolderC pf1 pt1) == Lib1.MoveFolder (Lib1.MoveFolderC pf2 pt2) = pf1 == pf2 && pt1 == pt2
  Lib1.DeleteFolder (Lib1.DeleteFolderC pf1) == Lib1.DeleteFolder (Lib1.DeleteFolderC pf2) = pf1 == pf2
  Lib1.Dump d1 == Lib1.Dump d2 = d1 == d2
  _ == _ = False
