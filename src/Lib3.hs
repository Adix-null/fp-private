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

import Control.Applicative (Alternative (empty, some, (<|>)))
import Control.Concurrent (Chan, readChan, writeChan)
import Control.Concurrent.Chan (newChan)
import Control.Concurrent.STM (atomically, readTVarIO)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Exception (SomeException, try)
import Data.Char (isAlpha, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf)
import qualified Lib1
import qualified Lib2 (toCliCommand)
import Text.Read ()

fileName :: String
fileName = "FSstate.txt"

newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Left e -> Left e
      Right (v, r) -> Right (f v, r)

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

-- Primitive parsers
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

-- parseExtension using parseOneOf then mapping
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

-- parseName: <alphanumstr> "." <extension>
parseName :: Parser Lib1.Name
parseName =
  (\nameStr _ ext -> Lib1.Name (Lib1.stringToAlphanumStr nameStr) ext)
    <$> parseAlphaNumStr
    <*> keyword "."
    <*> parseExtension

-- parseASCII: a single alphanum or symbol
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
    isSymbol ch = ch `elem` ['!', '\"', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', ';', ':', '<', '=', '>', '?', '@', '\\', '^', '_', '`', '{', '|', '}', '~']
    toSymbol ch = case ch of
      '!' -> Lib1.SymExclam
      '\"' -> Lib1.SymQuote
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

-- ASCII sequence
parseData :: Parser Lib1.Data
parseData = toData <$> some parseASCII
  where
    toData :: [Lib1.ASCII] -> Lib1.Data
    toData [a] = Lib1.SingleASCII a
    toData (a : rest) = Lib1.RecASCII a (toData rest)
    toData [] = error "parseData: impossible"

-- <name> "#" <data>
parseFile :: Parser Lib1.File
parseFile = (\name _ dat -> Lib1.File name dat) <$> parseName <*> keyword "#" <*> parseData

-- path /
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

-- fish, spaceships and p-maps
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

-- | Parses user's input.
-- Yes, this is pretty much the same parser as in Lib2
-- but with a bit different requirements:
-- 1) It must implement Functor, Applicative and Alternative
-- 2) It must NOT implement Monad, no do-notations
-- 3) pmap with andN become <$> <*>
-- 4) orElse becomes <|>
-- 5) many and many1 become many and some
-- Yes, it will be mostly a copy-paste but an easy one
-- if Lib2 was implemented correctly.
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

-- helper parsers
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

commandKeys :: Lib1.Command -> [String]
commandKeys cmd = case cmd of
  Lib1.AddFile path (Lib1.File (Lib1.Name an _ext) _dat) -> ["file:" ++ showPath path ++ "/" ++ showAlphanumStr an]
  Lib1.MoveFile from to (Lib1.Name an _ext) -> ["file:" ++ showPath from ++ "/" ++ showAlphanumStr an, "file:" ++ showPath to ++ "/" ++ showAlphanumStr an]
  Lib1.DeleteFile path (Lib1.Name an _ext) -> ["file:" ++ showPath path ++ "/" ++ showAlphanumStr an]
  Lib1.AddFolder path _name -> ["folder:" ++ showPath path]
  Lib1.MoveFolder from to -> ["folder:" ++ showPath from, "folder:" ++ showPath to]
  Lib1.DeleteFolder path -> ["folder:" ++ showPath path]
  Lib1.Dump _ -> ["dump"]

-- state and command file storage

-- | You can change the type to whatever needed. If your domain
-- does not have any state you have to make it up.
newtype State = State {unState :: [Lib1.Command]}

-- Fix this accordingly
emptyState :: State
emptyState = State []

computeNextState :: State -> Lib1.Command -> State
computeNextState (State old) cmd =
  let keys = commandKeys cmd
      keep c = null (keys `intersect` commandKeys c)
      newList = cmd : filter keep old
   in State newList
  where
    intersect a b = [x | x <- a, x `elem` b]

-- | Business/domain logic happens here.
-- This function makes your program actually usefull.
-- You may print if you want to print, you
-- may mutate state if needed but there must be
-- SINGLE atomically call in the function
-- You do not want to write/read files here.
execute :: TVar State -> Lib1.Command -> IO ()
execute tvar cmd = atomically $ do
  s <- readTVar tvar
  let s' = computeNextState s cmd
  writeTVar tvar s'

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request. It must run forever.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = do
  _ <- readChan c
  return $ error "Implement me 2"

-- | This function will be called periodically
-- and on programs' exit. File writes must be performed
-- through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save _ _ = return $ Right ()

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load _ _ = return $ Right ()
