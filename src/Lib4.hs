{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module Lib4 where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Control.Monad.Trans.State.Strict (State, state)
import Data.Char (isAlpha, isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf)
import qualified Lib1
import Test.QuickCheck (Arbitrary, Gen, arbitrary)

type ErrorMsg = String

type Input = String

type Parser = ExceptT ErrorMsg (State Input)

-- | Parses user's input.
-- Yes, yes, yes. This is pretty much the same parser as in Lib3
-- It will be mostly a copy-paste because all <|>, <$>, <*> work
-- out of the box and only terminal (leaves) parsers will be changed.
parseCommand :: Parser Lib1.Command
parseCommand =
  ( parseAddFile
      <|> parseMoveFile
      <|> parseDeleteFile
      <|> parseAddFolder
      <|> parseMoveFolder
      <|> parseDeleteFolder
      <|> parseAddFolderAtRoot
      <|> parseDump
      <|> parsePrintFS
      <|> parseNotImplemented
  )
    <* requireEnd

-- Basic parsers
parseLetter :: Parser Char
parseLetter = ExceptT $ state $ \s ->
  case s of
    [] -> (Left "A letter is expected but got empty input", [])
    (h : t) ->
      if isAlpha h
        then (Right h, t)
        else (Left ("A letter is expected, but got " ++ [h]), s)

parseDigit :: Parser Char
parseDigit = ExceptT $ state $ \s ->
  case s of
    [] -> (Left "A digit is expected but got empty input", [])
    (h : t) ->
      if isDigit h
        then
          (Right h, t)
        else
          (Left $ "A digit is expected, but got " ++ [h], s)

parseASCII :: Parser Lib1.ASCII
parseASCII = ExceptT $ state $ \s ->
  case s of
    [] -> (Left "Unexpected end of input", [])
    (c : cs)
      | isAzAZ09 c -> (Right (Lib1.Alphanum (toAzAZ09 c)), cs)
      | isSymbol c -> (Right (Lib1.Symbol (toSymbol c)), cs)
      | otherwise -> (Left $ "Unexpected character: " ++ [c], s)
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

parseAlphaNum :: Parser Char
parseAlphaNum = parseLetter <|> parseDigit

parseAlphaNumStr :: Parser String
parseAlphaNumStr = some parseAlphaNum

keyword :: String -> Parser String
keyword prefix = ExceptT $ state $ \input ->
  if prefix `isPrefixOf` input
    then (Right (drop (length prefix) input), input)
    else (Left $ prefix ++ " is expected, got " ++ take (min 30 (length input)) input, input)

whitespace :: Parser String
whitespace = concat <$> some (keyword " " <|> keyword "\t")

parseOneOf :: [String] -> Parser String
parseOneOf exts = foldr1 (<|>) (map keyword exts)

-- ASCII seq
parseData :: Parser Lib1.Data
parseData = toData <$> some parseASCII
  where
    toData :: [Lib1.ASCII] -> Lib1.Data
    toData [a] = Lib1.SingleASCII a
    toData (a : rest) = Lib1.RecASCII a (toData rest)
    toData [] = error "parseData: impossible"

-- <alphanumstr> "." <extension>
parseName :: Parser Lib1.Name
parseName =
  (\nameStr _ ext -> Lib1.Name (Lib1.stringToAlphanumStr nameStr) ext)
    <$> parseAlphaNumStr
    <*> keyword "."
    <*> parseExtension

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

-- path /
parsePath :: Parser Lib1.Path
parsePath =
  parseSinglePath <|> parseRecPath
  where
    parseSinglePath =
      Lib1.SinglePath . Lib1.stringToAlphanumStr <$> parseAlphaNumStr

    parseRecPath =
      (\seg _ rest -> Lib1.RecPath (Lib1.stringToAlphanumStr seg) rest)
        <$> parseAlphaNumStr
        <*> keyword "/"
        <*> parsePath

-- <name> "#" <data>
parseFile :: Parser Lib1.File
parseFile = (\name _ dat -> Lib1.File name dat) <$> parseName <*> keyword "#" <*> parseData

-- Commands
parseAddFile :: Parser Lib1.Command
parseAddFile =
  (\_ _ path _ file -> Lib1.AddFile path file)
    <$> keyword "AddFile"
    <*> whitespace
    <*> parsePath
    <*> whitespace
    <*> parseFile

parseMoveFile :: Parser Lib1.Command
parseMoveFile =
  (\_ _ from _ to _ name -> Lib1.MoveFile from to name)
    <$> keyword "MoveFile"
    <*> whitespace
    <*> parsePath
    <*> whitespace
    <*> parsePath
    <*> whitespace
    <*> parseName

parseDeleteFile :: Parser Lib1.Command
parseDeleteFile =
  (\_ _ path _ name -> Lib1.DeleteFile path name)
    <$> keyword "DeleteFile"
    <*> whitespace
    <*> parsePath
    <*> whitespace
    <*> parseName

parseAddFolder :: Parser Lib1.Command
parseAddFolder =
  (\_ _ path _ folderNameStr -> Lib1.AddFolder path (Lib1.stringToAlphanumStr folderNameStr))
    <$> keyword "AddFolder"
    <*> whitespace
    <*> parsePath
    <*> whitespace
    <*> parseAlphaNumStr

parseMoveFolder :: Parser Lib1.Command
parseMoveFolder =
  (\_ _ from _ to -> Lib1.MoveFolder from to)
    <$> keyword "MoveFolder"
    <*> whitespace
    <*> parsePath
    <*> whitespace
    <*> parsePath

parseDeleteFolder :: Parser Lib1.Command
parseDeleteFolder =
  (\_ _ path -> Lib1.DeleteFolder path)
    <$> keyword "DeleteFolder"
    <*> whitespace
    <*> parsePath

parseAddFolderAtRoot :: Parser Lib1.Command
parseAddFolderAtRoot =
  ( \_ _ folderNameStr ->
      Lib1.AddFolderAtRoot (Lib1.stringToAlphanumStr folderNameStr)
  )
    <$> keyword "AddFolderAtRoot"
    <*> whitespace
    <*> parseAlphaNumStr

parseDump :: Parser Lib1.Command
parseDump = (\_ _ d -> Lib1.Dump d) <$> keyword "Dump" <*> whitespace <*> parseDumpable

parseDumpable :: Parser Lib1.Dumpable
parseDumpable = Lib1.Examples <$ keyword "Examples"

parsePrintFS :: Parser Lib1.Command
parsePrintFS = Lib1.PrintFS <$ keyword "PrintFS"

parseNotImplemented :: Parser Lib1.Command
parseNotImplemented = ExceptT $ state $ \_ -> (Left "Not implemented", [])

requireEnd :: Parser ()
requireEnd = ExceptT $ state $ \input ->
  if all (`elem` [' ', '\t']) input
    then (Right (), [])
    else (Left $ "Whitespace at the end required: " ++ show input, input)

-- | This generates arbitrary (a.k.a random) commands for tests.
instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary = error "Implement me"
