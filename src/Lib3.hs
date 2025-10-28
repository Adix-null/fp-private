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
import qualified Lib1

newtype Parser a = Parser
  { runParser :: String -> Either String (a, String)
  }

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

-- pmap, andN
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f functor = Parser $ \input ->
    case runParser functor input of
      Left e -> Left e
      Right (v, r) -> Right (f v, r)

-- orElse
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left "No alternatives"
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      Right r2 -> Right r2
      Left e1 ->
        case runParser p2 input of
          Right r2 -> Right r2
          Left e2 -> Left $ e1 ++ "; " ++ e2

-- aplicative functor
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  af <*> aa = Parser $ \input ->
    case runParser af input of
      Left e1 -> Left e1
      Right (f, r1) ->
        case runParser aa r1 of
          Left e2 -> Left e2
          Right (a, r2) -> Right (f a, r2)

parseCommand :: Parser Lib1.Command
parseCommand = Parser $ \_ -> Left "Implement me 0"

-- | You can change the type to whatever needed. If your domain
-- does not have any state you have to make it up.
newtype State = State ()

-- Fix this accordingly
emptyState :: State
emptyState = State ()

-- | Business/domain logic happens here.
-- This function makes your program actually usefull.
-- You may print if you want to print, you
-- may mutate state if needed but there must be
-- SINGLE atomically call in the function
-- You do not want to write/read files here.
execute :: TVar State -> Lib1.Command -> IO ()
execute _ _ = error "Implement me 1"

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
save _ _ = return $ Left "Implement me 3"

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load _ _ = return $ Left "Implement me 4"
