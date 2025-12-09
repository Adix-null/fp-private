{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Lib4 where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.State.Strict (State, state)
import Data.Char (isAlpha, isAsciiLower, isAsciiUpper, isDigit)
import Control.Monad.Trans.State.Strict (runState)
import Data.List (isPrefixOf)
import Control.Monad.Trans.Except (runExceptT)
import qualified Lib1
import Test.QuickCheck (Arbitrary, arbitrary)
import Test.QuickCheck.Gen
import Debug.Trace

type ErrorMsg = String

type Input = String

type Parser = ExceptT ErrorMsg (State Input)

try :: Parser a -> Parser a
try p = ExceptT $ state $ \s ->
  case runState (runExceptT p) s of
    (Left _, _) -> (Left "try failed", s) -- reset state on fail
    r -> r


-- | Parses user's input.
-- Yes, yes, yes. This is pretty much the same parser as in Lib3
-- It will be mostly a copy-paste because all <|>, <$>, <*> work
-- out of the box and only terminal (leaves) parsers will be changed.
parseCommand :: Parser Lib1.Command
parseCommand =
  ( parseAddFile
      <|> parseAddFolderAtRoot
      <|> parseMoveFile
      <|> parseDeleteFile
      <|> parseAddFolder
      <|> parseMoveFolder
      <|> parseDeleteFolder
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
  -- traceShow ("keyword trying: " ++ show prefix ++ " on input: " ++ show input) $
    if prefix `isPrefixOf` input
      then (Right prefix, drop (length prefix) input)
      else (Left $ "Expected: " ++ prefix ++ ", Got: " ++ take (min 30 (length input)) input ++ "; ", input)

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
      | all (`elem` (['a' .. 'z'] ++ ['0' .. '9'])) s =
          case lookup s Lib1.extensions of
            Just ext -> ext
            Nothing -> error $ "Ext not in list: " ++ s
      | otherwise = error $ "Ext must be alphanumerical: " ++ s


-- path /
parsePath :: Parser Lib1.Path
parsePath = 
  try parseRecPath <|> parseSinglePath
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
  (\_ _ folderNameStr -> Lib1.AddFolderAtRoot (Lib1.stringToAlphanumStr folderNameStr))
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
  if not (any (`notElem` [' ', '\t']) input)
    then (Right (), [])
    else (Left $ "Unexpected trailing characters: " ++ show input, input)


-- | This generates arbitrary (a.k.a random) commands for tests.
instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary =
    oneof
      [ 
        Lib1.AddFile <$> randomPath 3 <*> randomFile,
        Lib1.MoveFile <$> randomPath 3 <*> randomPath 3 <*> randomName,
        Lib1.DeleteFile <$> randomPath 3 <*> randomName,
        Lib1.AddFolder <$> randomPath 3 <*> randomAlphaNumStr,
        Lib1.MoveFolder <$> randomPath 3 <*> randomPath 3,
        Lib1.DeleteFolder <$> randomPath 3,
        Lib1.AddFolderAtRoot <$> randomAlphaNumStr,
        pure (Lib1.Dump Lib1.Examples),
        pure Lib1.PrintFS
      ]
    where
      randomPath :: Int -> Gen Lib1.Path
      randomPath 0 = Lib1.SinglePath <$> randomAlphaNumStr
      randomPath n = oneof [Lib1.SinglePath <$> randomAlphaNumStr, Lib1.RecPath <$> randomAlphaNumStr <*> randomPath (n - 1)]
      
      randomFile :: Gen Lib1.File
      randomFile = Lib1.File <$> randomName <*> randomData

      randomName :: Gen Lib1.Name
      randomName = Lib1.Name <$> randomAlphaNumStr <*> elements (map snd Lib1.extensions)

      randomAlphaNumStr :: Gen Lib1.AlphanumStr
      randomAlphaNumStr = Lib1.stringToAlphanumStr <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

      randomData :: Gen Lib1.Data
      randomData = do
        a <- randomASCII
        b <- randomASCII
        c <- randomASCII
        d <- randomASCII
        return $ Lib1.RecASCII a (Lib1.RecASCII b (Lib1.RecASCII c (Lib1.SingleASCII d)))

      randomASCII :: Gen Lib1.ASCII
      randomASCII =
        oneof
          [ Lib1.Alphanum . Lib1.Upper <$> elements ['A' .. 'Z'],
            Lib1.Alphanum . Lib1.Lower <$> elements ['a' .. 'z'],
            Lib1.Alphanum . Lib1.Digit <$> elements ['0' .. '9'],
            elements
              [ Lib1.Symbol Lib1.SymExclam,
                Lib1.Symbol Lib1.SymQuote,
                Lib1.Symbol Lib1.SymDollar,
                Lib1.Symbol Lib1.SymPercent,
                Lib1.Symbol Lib1.SymAmpersand,
                Lib1.Symbol Lib1.SymApostrophe,
                Lib1.Symbol Lib1.SymLParen,
                Lib1.Symbol Lib1.SymRParen,
                Lib1.Symbol Lib1.SymAsterisk,
                Lib1.Symbol Lib1.SymPlus,
                Lib1.Symbol Lib1.SymComma,
                Lib1.Symbol Lib1.SymMinus,
                Lib1.Symbol Lib1.SymDot,
                Lib1.Symbol Lib1.SymColon,
                Lib1.Symbol Lib1.SymSemicolon,
                Lib1.Symbol Lib1.SymLt,
                Lib1.Symbol Lib1.SymEq,
                Lib1.Symbol Lib1.SymGt,
                Lib1.Symbol Lib1.SymQMark,
                Lib1.Symbol Lib1.SymAt,
                Lib1.Symbol Lib1.SymBackslash,
                Lib1.Symbol Lib1.SymCaret,
                Lib1.Symbol Lib1.SymUnderscore,
                Lib1.Symbol Lib1.SymBacktick,
                Lib1.Symbol Lib1.SymLCurly,
                Lib1.Symbol Lib1.SymPipe,
                Lib1.Symbol Lib1.SymRCurly,
                Lib1.Symbol Lib1.SymTilde
              ]
          ]
