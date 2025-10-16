{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib2
  ( parseCommand,
    ToCliCommand (..),
    process,
  )
where

import Data.Char (isAlpha, isDigit)
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

--
parseOneOf :: [String] -> Parser ErrorMsg String
parseOneOf exts = foldr1 orElse (map keyword exts)

parseExt :: Parser ErrorMsg String
parseExt = parseOneOf ["txt", "png", "jpg", "json", "dat", "exe", "hs", "cs", "html", "cpp", "mp4", "mp3"]

parseFilename :: Parser ErrorMsg String
parseFilename =
  pmap (\(name, _dot, ext) -> name ++ "." ++ ext) $
    and3 parseAlphaNumStr (keyword ".") parseExt

parseEditFile :: Parser ErrorMsg Lib1.Command
parseEditFile input =
  case and3 (keyword "EditFile") ws parseFilename input of
    Left e -> Left e
    Right ((_, _, fname), rest) ->
      if null rest
        then Right (Lib1.EditFile fname, rest)
        else Left $ "Bad extension name " ++ rest

parseCommand :: Parser ErrorMsg Lib1.Command
parseCommand = parseEditFile

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "EXAMPLES:" : map toCliCommand Lib1.examples
process (Lib1.EditFile "test.txt") = ["EXAMPLE EDIT: test.txt"]
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.EditFile fname) = "EditFile " ++ fname
  toCliCommand (Lib1.Dump d) = "Dump " ++ show d

-- Eq instance
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.EditFile f1 == Lib1.EditFile f2 = f1 == f2
  _ == _ = False
