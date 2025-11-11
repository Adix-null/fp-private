{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib3
  ( emptyState,
    State (..),
    execute,
    load,
    save,
    storageOpLoop,
    StorageOp,
    Parser (..),
    parseCommand,
  )
where

import Control.Applicative
import Control.Concurrent (Chan, readChan)
import Control.Concurrent.STM.TVar (TVar)
import Data.Char (isAlpha, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf)
import qualified Lib1

newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

-- pmap, andN
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Left e -> Left e
      Right (v, r) -> Right (f v, r)

-- orElse
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "No alternatives"

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      Right r -> Right r
      Left e1 ->
        case runParser p2 input of
          Right r2 -> Right r2
          Left e2 -> Left $ e1 ++ "; " ++ e2

-- aplicative functor
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = Parser $ \input ->
    case runParser pf input of
      Left e1 -> Left e1
      Right (f, rest1) ->
        case runParser pa rest1 of
          Left e2 -> Left e2
          Right (a, rest2) -> Right (f a, rest2)

parseLetter :: Parser Char
parseLetter = Parser $ \input ->
  case input of
    [] -> Left "A letter is expected but got empty input"
    (h : t) -> if isAlpha h then Right (h, t) else Left $ "A letter is expected, but got " ++ [h]

parseDigit :: Parser Char
parseDigit = Parser $ \input ->
  case input of
    [] -> Left "A digit is expected but got empty input"
    (h : t) -> if isDigit h then Right (h, t) else Left $ "A digit is expected, but got " ++ [h]

parseAlphaNum :: Parser Char
parseAlphaNum = parseLetter <|> parseDigit

parseAlphaNumStr :: Parser String
parseAlphaNumStr = some parseAlphaNum

keyword :: String -> Parser String
keyword prefix = Parser $ \input ->
  if prefix `isPrefixOf` input
    then Right (prefix, drop (length prefix) input)
    else Left $ prefix ++ " is expected, got " ++ take (min 30 (length input)) input

ws :: Parser String
ws = concat <$> some (keyword " " <|> keyword "\t")

parseOneOf :: [String] -> Parser String
parseOneOf exts = foldr1 (<|>) (map keyword exts)

parseExtension :: Parser Lib1.Extension
parseExtension = toExt <$> parseOneOf (map fst Lib1.extensions)
  where
    toExt :: String -> Lib1.Extension
    toExt s
      | all (`elem` ['a' .. 'z']) s =
          case lookup s Lib1.extensions of
            Just ext -> ext
            Nothing -> error $ "Ext not in list: " ++ s
      | otherwise = error $ "Ext must be lowercase: " ++ s

parseName :: Parser Lib1.Name
parseName =
  (\nameStr _ ext -> Lib1.Name (Lib1.stringToAlphanumStr nameStr) ext)
    <$> parseAlphaNumStr
    <*> keyword "."
    <*> parseExtension

parseASCII :: Parser Lib1.ASCII
parseASCII = Parser $ \input ->
  case input of
    [] -> Left "Unexpected end of input"
    (c : cs)
      | isAzAZ09 c -> Right (Lib1.Alphanum (toAzAZ09 c), cs)
      | isSymbol c -> Right (Lib1.Symbol (toSymbol c), cs)
      | otherwise -> Left $ "Unexpected character: " ++ show c
  where
    isAzAZ09 ch = isAsciiUpper ch || isAsciiLower ch || isDigit ch
    toAzAZ09 ch
      | isAsciiUpper ch = Lib1.Upper ch
      | isAsciiLower ch = Lib1.Lower ch
      | isDigit ch = Lib1.Digit ch
      | otherwise = error "Shouldn't be possible"
    isSymbol ch = ch `elem` ['!', '"', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', ';', ':', '<', '=', '>', '?', '@', '\\', '^', '_', '`', '{', '|', '}', '~']
    toSymbol ch = case ch of
      '!' -> Lib1.SymExclam
      '"' -> Lib1.SymQuote
      '$' -> Lib1.SymDollar
      '%' -> Lib1.SymPercent
      '&' -> Lib1.SymAmpersand
      '\'' -> Lib1.SymApostrophe
      '(' -> Lib1.SymLParen
      ')' -> Lib1.SymRParen
      '*' -> Lib1.SymAsterisk
      '+' -> Lib1.SymPlus
      ',' -> Lib1.SymComma
      '-' -> Lib1.SymMinus
      '.' -> Lib1.SymDot
      ':' -> Lib1.SymColon
      ';' -> Lib1.SymSemicolon
      '<' -> Lib1.SymLt
      '=' -> Lib1.SymEq
      '>' -> Lib1.SymGt
      '?' -> Lib1.SymQMark
      '@' -> Lib1.SymAt
      '\\' -> Lib1.SymBackslash
      '^' -> Lib1.SymCaret
      '_' -> Lib1.SymUnderscore
      '`' -> Lib1.SymBacktick
      '{' -> Lib1.SymLCurly
      '|' -> Lib1.SymPipe
      '}' -> Lib1.SymRCurly
      '~' -> Lib1.SymTilde
      _ -> error $ "Unexpected symbol " ++ show ch

parseData :: Parser Lib1.Data
parseData = toData <$> some parseASCII
  where
    toData :: [Lib1.ASCII] -> Lib1.Data
    toData [a] = Lib1.SingleASCII a
    toData (a : rest) = Lib1.RecASCII a (toData rest)
    toData [] = error "parseData: impossible"

parseFile :: Parser Lib1.File
parseFile = (\name _ dat -> Lib1.File name dat) <$> parseName <*> keyword "#" <*> parseData

parsePath :: Parser Lib1.Path
parsePath = Parser $ \input ->
  case runParser parseAlphaNumStr input of
    Left e -> Left e
    Right (seg, rest) ->
      let alphanum = Lib1.stringToAlphanumStr seg
       in case rest of
            ('/' : r) ->
              case runParser parsePath r of
                Left e2 -> Left e2
                Right (p, remaining) -> Right (Lib1.RecPath alphanum p, remaining)
            _ -> Right (Lib1.SinglePath alphanum, rest)

parseDumpable :: Parser Lib1.Dumpable
parseDumpable = Lib1.Examples <$ keyword "Examples"

parseAddFile :: Parser Lib1.Command
parseAddFile =
  (\_ _ path _ file -> Lib1.AddFile path file)
    <$> keyword "AddFile"
    <*> ws
    <*> parsePath
    <*> ws
    <*> parseFile

parseMoveFile :: Parser Lib1.Command
parseMoveFile =
  (\_ _ from _ to _ name -> Lib1.MoveFile from to name)
    <$> keyword "MoveFile"
    <*> ws
    <*> parsePath
    <*> ws
    <*> parsePath
    <*> ws
    <*> parseName

parseDeleteFile :: Parser Lib1.Command
parseDeleteFile =
  (\_ _ path _ name -> Lib1.DeleteFile path name)
    <$> keyword "DeleteFile"
    <*> ws
    <*> parsePath
    <*> ws
    <*> parseName

parseAddFolder :: Parser Lib1.Command
parseAddFolder =
  (\_ _ path _ folderNameStr -> Lib1.AddFolder path (Lib1.stringToAlphanumStr folderNameStr))
    <$> keyword "AddFolder"
    <*> ws
    <*> parsePath
    <*> ws
    <*> parseAlphaNumStr

parseMoveFolder :: Parser Lib1.Command
parseMoveFolder =
  (\_ _ from _ to -> Lib1.MoveFolder from to)
    <$> keyword "MoveFolder"
    <*> ws
    <*> parsePath
    <*> ws
    <*> parsePath

parseDeleteFolder :: Parser Lib1.Command
parseDeleteFolder =
  (\_ _ path -> Lib1.DeleteFolder path)
    <$> keyword "DeleteFolder"
    <*> ws
    <*> parsePath

parseDump :: Parser Lib1.Command
parseDump = (\_ _ d -> Lib1.Dump d) <$> keyword "Dump" <*> ws <*> parseDumpable

requireEnd :: Parser ()
requireEnd = Parser $ \input ->
  if all (`elem` [' ', '\t']) input
    then Right ((), "")
    else Left $ "Whitespace at the end required: " ++ show input

parseNotImplemented :: Parser Lib1.Command
parseNotImplemented = Parser $ \_ -> Left "Not implemented"

parseCommand :: Parser Lib1.Command
parseCommand =
  ( parseAddFile
      <|> parseMoveFile
      <|> parseDeleteFile
      <|> parseAddFolder
      <|> parseMoveFolder
      <|> parseDeleteFolder
      <|> parseDump
      <|> parseNotImplemented
  )
    <* requireEnd

newtype State = State ()

emptyState :: State
emptyState = State ()

execute :: TVar State -> Lib1.Command -> IO ()
execute _ _ = error "Implement me 1"

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = do
  _ <- readChan c
  return $ error "Implement me 2"

save :: Chan StorageOp -> TVar State -> IO (Either String ())
save _ _ = return $ Left "Implement me 3"

load :: Chan StorageOp -> TVar State -> IO (Either String ())
load _ _ = return $ Left "Implement me 4"
