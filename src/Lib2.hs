{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib2
  ( parseCommand,
    ToCliCommand (..),
    process,
  )
where

import Data.Char (isAlpha, isDigit, isLower, isUpper)
import Data.List (isPrefixOf)
import qualified Lib1

type ErrorMsg = String

type Parser a = String -> Either ErrorMsg (a, String)

-- lesson helper utility stuff
parseLetter :: Parser Char
parseLetter [] = Left "A letter is expected but got empty input"
parseLetter (h : t) =
  if isAlpha h
    then Right (h, t)
    else Left $ "A letter is expected, but got " ++ [h]

parseDigit :: Parser Char
parseDigit [] = Left "A digit is expected but got empty input"
parseDigit (h : t) =
  if isDigit h
    then Right (h, t)
    else Left $ "A digit is expected, but got " ++ [h]

parseAlphaNum :: Parser Char
parseAlphaNum input =
  case parseLetter input of
    Right r -> Right r
    Left _ -> parseDigit input

many :: Parser a -> Parser [a]
many p = many' p []
  where
    many' p' acc input =
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

many1 :: Parser a -> Parser [a]
many1 p input =
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left "At least on value required"
    Right a -> Right a

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p input =
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)

keyword :: String -> Parser String
keyword prefix input
  | prefix `isPrefixOf` input = Right (prefix, drop (length prefix) input)
  | otherwise = Left $ prefix ++ " is expected, got " ++ input

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input =
  case p1 input of
    Right r1 -> Right r1
    Left e1 ->
      case p2 input of
        Right r2 -> Right r2
        Left e2 -> Left $ e1 <> e2

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
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

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 pa pb pc pd input =
  case and3 pa pb pc input of
    Left err -> Left err
    Right ((va, vb, vc), r) ->
      case pd r of
        Left err -> Left err
        Right (vd, r2) -> Right ((va, vb, vc, vd), r2)

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a, b, c, d, e)
and5 pa pb pc pd pe input =
  case and4 pa pb pc pd input of
    Left err -> Left err
    Right ((va, vb, vc, vd), r) ->
      case pe r of
        Left err -> Left err
        Right (ve, r2) -> Right ((va, vb, vc, vd, ve), r2)

and6 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser (a, b, c, d, e, f)
and6 pa pb pc pd pe pf input =
  case and5 pa pb pc pd pe input of
    Left err -> Left err
    Right ((va, vb, vc, vd, ve), r) ->
      case pf r of
        Left err -> Left err
        Right (vf, r2) -> Right ((va, vb, vc, vd, ve, vf), r2)

and7 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser (a, b, c, d, e, f, g)
and7 pa pb pc pd pe pf pg input =
  case and6 pa pb pc pd pe pf input of
    Left err -> Left err
    Right ((va, vb, vc, vd, ve, vf), r) ->
      case pg r of
        Left err -> Left err
        Right (vg, r2) -> Right ((va, vb, vc, vd, ve, vf, vg), r2)

ws :: Parser String
ws = pmap concat (many1 (keyword " " `orElse` keyword "\t"))

-- custom helper parsers
parseAlphaNumStr :: Parser String
parseAlphaNumStr = many1 parseAlphaNum

parseOneOf :: [String] -> Parser String
parseOneOf exts = foldr1 orElse (map keyword exts)

parseFile :: Parser Lib1.File
parseFile =
  pmap
    (\(name, _, dat) -> Lib1.File name dat)
    (and3 parseName (keyword "#") parseData)

parsePath :: Parser Lib1.Path
parsePath input =
  case parseAlphaNumStr input of
    Left e -> Left e
    Right (seg, rest) ->
      let alphanum = Lib1.stringToAlphanumStr seg
       in case rest of
            ('/' : r) ->
              case parsePath r of
                Left e2 -> Left e2
                Right (p, remaining) -> Right (Lib1.RecPath alphanum p, remaining)
            _ -> Right (Lib1.SinglePath alphanum, rest) -- when end of path reached

parseName :: Parser Lib1.Name
parseName input =
  case and3 parseAlphaNumStr (keyword ".") parseExtension input of
    Left e -> Left e
    Right ((nameStr, _, ext), remaining) ->
      Right (Lib1.Name (Lib1.stringToAlphanumStr nameStr) ext, remaining)

parseExtension :: Parser Lib1.Extension
parseExtension =
  pmap toExt (parseOneOf (map fst Lib1.extensions))
  where
    toExt :: String -> Lib1.Extension
    toExt s = case lookup s Lib1.extensions of
      Just ext -> ext
      Nothing -> error $ "Impossible: extension not in list: " ++ s

parseSymbol :: Parser Lib1.Symbol
parseSymbol [] = Left "Expected symbol but got empty input"
parseSymbol (c : cs) =
  case c of
    '\t' -> Left "tab not allowed"
    '\n' -> Left "newline not allowed"
    '#' -> Left "# not allowed"
    '!' -> Right (Lib1.SymExclam, cs)
    '"' -> Right (Lib1.SymQuote, cs)
    '$' -> Right (Lib1.SymDollar, cs)
    '%' -> Right (Lib1.SymPercent, cs)
    '&' -> Right (Lib1.SymAmpersand, cs)
    '\'' -> Right (Lib1.SymApostrophe, cs)
    '(' -> Right (Lib1.SymLParen, cs)
    ')' -> Right (Lib1.SymRParen, cs)
    '*' -> Right (Lib1.SymAsterisk, cs)
    '+' -> Right (Lib1.SymPlus, cs)
    ',' -> Right (Lib1.SymComma, cs)
    '-' -> Right (Lib1.SymMinus, cs)
    '.' -> Right (Lib1.SymDot, cs)
    ':' -> Right (Lib1.SymColon, cs)
    ';' -> Right (Lib1.SymSemicolon, cs)
    '<' -> Right (Lib1.SymLt, cs)
    '=' -> Right (Lib1.SymEq, cs)
    '>' -> Right (Lib1.SymGt, cs)
    '?' -> Right (Lib1.SymQMark, cs)
    '@' -> Right (Lib1.SymAt, cs)
    '\\' -> Right (Lib1.SymBackslash, cs)
    '^' -> Right (Lib1.SymCaret, cs)
    '_' -> Right (Lib1.SymUnderscore, cs)
    '`' -> Right (Lib1.SymBacktick, cs)
    '{' -> Right (Lib1.SymLCurly, cs)
    '|' -> Right (Lib1.SymPipe, cs)
    '}' -> Right (Lib1.SymRCurly, cs)
    '~' -> Right (Lib1.SymTilde, cs)
    _ -> Left $ "Invalid symbol: " ++ [c]

parseASCII :: Parser Lib1.ASCII
parseASCII input =
  case parseAlphaNum input of
    Right (c, rest) ->
      let az
            | isLower c = Lib1.Lower c
            | isUpper c = Lib1.Upper c
            | otherwise = Lib1.Digit c
       in Right (Lib1.ASCII az Lib1.SymTab, rest) -- alphanum with dummy SymTab
    Left _ ->
      case parseSymbol input of
        Right (sym, rest) -> Right (Lib1.ASCII (Lib1.Digit '0') sym, rest) -- symbol with dummy AzAZ09
        Left e -> Left e

parseData :: Parser Lib1.Data
parseData input =
  case parseASCII input of
    Left e -> Left e
    Right (ascii, rest) ->
      case parseData rest of
        Left _ -> Right (Lib1.SingleASCII ascii, rest) -- end of data
        Right (d, remaining) -> Right (Lib1.RecASCII ascii d, remaining)

parseAddFile :: Parser Lib1.Command
parseAddFile =
  requireEnd $
    pmap
      (\(_, _, path, _, file) -> Lib1.AddFile path file)
      (and5 (keyword "AddFile") ws parsePath ws parseFile)

parseMoveFile :: Parser Lib1.Command
parseMoveFile =
  pmap
    ( \(_, _ws1, path_from, _ws2, path_to, _ws3, flname) ->
        Lib1.MoveFile path_from path_to flname
    )
    (and7 (keyword "MoveFile") ws parsePath ws parsePath ws parseName)

parseDeleteFile :: Parser Lib1.Command
parseDeleteFile =
  pmap
    ( \(_, _ws1, path, _ws2, fname) ->
        Lib1.DeleteFile path fname
    )
    (and5 (keyword "DeleteFile") ws parsePath ws parseName)

parseAddFolder :: Parser Lib1.Command
parseAddFolder =
  pmap
    ( \(_, _ws1, path, _ws2, folderNameStr) ->
        Lib1.AddFolder path (Lib1.stringToAlphanumStr folderNameStr)
    )
    (and5 (keyword "AddFolder") ws parsePath ws parseAlphaNumStr)

parseMoveFolder :: Parser Lib1.Command
parseMoveFolder =
  pmap
    ( \(_, _ws1, path_from, _ws2, path_to) ->
        Lib1.MoveFolder path_from path_to
    )
    (and5 (keyword "MoveFolder") ws parsePath ws parsePath)

parseDeleteFolder :: Parser Lib1.Command
parseDeleteFolder =
  pmap
    ( \(_, _ws1, path) ->
        Lib1.DeleteFolder path
    )
    (and3 (keyword "DeleteFolder") ws parsePath)

requireEnd :: Parser a -> Parser a
requireEnd p input =
  case p input of
    Left e -> Left e
    Right (result, rest) ->
      case dropWhile (== ' ') rest of
        "" -> Right (result, "")
        leftover -> Left $ "Unexpected input after command: " ++ show leftover

parseNotImplemented :: Parser Lib1.Command
parseNotImplemented _ = Left "Not implemented"

-- | Parses user's input.
-- The function must be implemented and must have tests.
parseCommand :: Parser Lib1.Command
parseCommand = requireEnd (parseAddFile `orElse` parseMoveFile `orElse` parseDeleteFile `orElse` parseAddFolder `orElse` parseMoveFolder `orElse` parseDeleteFolder `orElse` parseNotImplemented)

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "EXAMPLES:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

-- | You have to make your Command an instance of ToCliCommand class.
-- Please remove all custom Show instances of Command ADT and
-- use "deriving Show" only.
instance ToCliCommand Lib1.Command where
  toCliCommand (Lib1.Dump d) = "Dump " ++ show d
  toCliCommand (Lib1.AddFile path file) = "AddFile " ++ showPath path ++ " " ++ showFile file
  toCliCommand (Lib1.MoveFile from to fname) = "MoveFile " ++ showPath from ++ " " ++ showPath to ++ " " ++ showName fname
  toCliCommand (Lib1.DeleteFile path fname) = "DeleteFile " ++ showPath path ++ " " ++ showName fname
  toCliCommand (Lib1.AddFolder path folderName) = "AddFolder " ++ showPath path ++ " " ++ showFolderName folderName
  toCliCommand (Lib1.MoveFolder from to) = "MoveFolder " ++ showPath from ++ " " ++ showPath to
  toCliCommand (Lib1.DeleteFolder path) = "DeleteFolder " ++ showPath path

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
  Lib1.AddFile pf1 d1 == Lib1.AddFile pf2 d2 = pf1 == pf2 && d1 == d2
  Lib1.MoveFile pf1 pt1 fln1 == Lib1.MoveFile pf2 pt2 fln2 = pf1 == pf2 && pt1 == pt2 && fln1 == fln2
  Lib1.DeleteFile pf1 fln1 == Lib1.DeleteFile pf2 fln2 = pf1 == pf2 && fln1 == fln2
  Lib1.AddFolder pf1 fdn1 == Lib1.AddFolder pf2 fdn2 = pf1 == pf2 && fdn1 == fdn2
  Lib1.MoveFolder pf1 pt1 == Lib1.MoveFolder pf2 pt2 = pf1 == pf2 && pt1 == pt2
  Lib1.DeleteFolder pf1 == Lib1.DeleteFolder pf2 = pf1 == pf2
  _ == _ = False
