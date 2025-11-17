{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import System.IO.Unsafe (unsafePerformIO)
import Text.Read ()

fileName :: String
fileName = "FSstate.txt"

newtype Parser a = Parser {runParser :: String -> Either String (a, String)}

-- fish, spaceship, p-map
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

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input ->
    case runParser p input of
      Left e -> Left e
      Right (v, r) -> Right (f v, r)

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

-- subparsers
parseLetter :: Parser Char
parseLetter = Parser $ \case
  [] -> Left "A letter is expected but got empty input"
  (h : t) ->
    if isAlpha h
      then
        Right (h, t)
      else
        Left $ "A letter is expected, but got " ++ [h]

parseDigit :: Parser Char
parseDigit = Parser $ \case
  [] -> Left "A digit is expected but got empty input"
  (h : t) ->
    if isDigit h
      then
        Right (h, t)
      else
        Left $ "A digit is expected, but got " ++ [h]

parseAlphaNum :: Parser Char
parseAlphaNum = parseLetter <|> parseDigit

parseAlphaNumStr :: Parser String
parseAlphaNumStr = some parseAlphaNum

keyword :: String -> Parser String
keyword prefix = Parser $ \input ->
  if prefix `isPrefixOf` input
    then Right (prefix, drop (length prefix) input)
    else Left $ prefix ++ " is expected, got " ++ take (min 30 (length input)) input

whitespace :: Parser String
whitespace = concat <$> some (keyword " " <|> keyword "\t")

parseOneOf :: [String] -> Parser String
parseOneOf exts = foldr1 (<|>) (map keyword exts)

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

parseASCII :: Parser Lib1.ASCII
parseASCII = Parser $ \case
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

-- ASCII seq
parseData :: Parser Lib1.Data
parseData = toData <$> some parseASCII
  where
    toData :: [Lib1.ASCII] -> Lib1.Data
    toData [a] = Lib1.SingleASCII a
    toData (a : rest) = Lib1.RecASCII a (toData rest)
    toData [] = error "parseData: impossible"

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

parseDump :: Parser Lib1.Command
parseDump = (\_ _ d -> Lib1.Dump d) <$> keyword "Dump" <*> whitespace <*> parseDumpable

parseDumpable :: Parser Lib1.Dumpable
parseDumpable = Lib1.Examples <$ keyword "Examples"

parseDumpHier :: Parser Lib1.Command
parseDumpHier = Lib1.DumpHierarchy <$ keyword "DumpHier"

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
      <|> parseDumpHier
      <|> parseNotImplemented
  )
    <* requireEnd

-- helper show functions
showName :: Lib1.Name -> String
showName (Lib1.Name an ext) = showAlphanumStr an ++ "." ++ show ext

showData :: Lib1.Data -> String
showData (Lib1.SingleASCII a) = showASCII a
showData (Lib1.RecASCII a rest) = showASCII a ++ showData rest

showASCII :: Lib1.ASCII -> String
showASCII (Lib1.Alphanum az) = showAzAZ09 az
showASCII (Lib1.Symbol sym) = show sym

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
  Lib1.DumpHierarchy -> ["dumpHierarchy"]

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

-- In-memory file system visual representation as tree
data Tree = Tree {tName :: String, tFolders :: [Tree], tFiles :: [String]}
  deriving (Show, Eq)

emptyTree :: Tree
emptyTree = Tree "" [] []

pathToSegments :: Lib1.Path -> [String]
pathToSegments p = go p []
  where
    go (Lib1.SinglePath a) acc = acc ++ [showAlphanumStr a]
    go (Lib1.RecPath a rest) acc = go rest (acc ++ [showAlphanumStr a])

findOrCreate :: String -> [Tree] -> (Tree, [Tree])
findOrCreate name [] = (Tree name [] [], [])
findOrCreate name (x : xs)
  | tName x == name = (x, xs)
  | otherwise =
      let (found, rest) = findOrCreate name xs
       in (found, x : rest)

insertFolderAt :: [String] -> String -> Tree -> Tree
insertFolderAt [] newName (Tree n fs f) =
  if any ((== newName) . tName) fs then Tree n fs f else Tree n (fs ++ [Tree newName [] []]) f
insertFolderAt (seg : segs) newName (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      child' = insertFolderAt segs newName child
      fs' = child' : rest
   in Tree n fs' f

insertFileAt :: [String] -> String -> Tree -> Tree
insertFileAt [] fname (Tree n fs f) =
  if fname `elem` f then Tree n fs f else Tree n fs (f ++ [fname])
insertFileAt (seg : segs) fname (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      child' = insertFileAt segs fname child
      fs' = child' : rest
   in Tree n fs' f

removeFolderAt :: [String] -> Tree -> Tree
removeFolderAt [] t = t
removeFolderAt [seg] (Tree n fs f) = Tree n (filter ((/= seg) . tName) fs) f
removeFolderAt (seg : segs) (Tree n fs f) =
  let (child, _) = findOrCreate seg fs
      child' = removeFolderAt segs child
   in Tree n (map (\c -> if tName c == tName child then child' else c) fs) f

moveFolder :: [String] -> [String] -> Tree -> Tree
moveFolder src dest tree =
  case extractFolder src tree of
    (Nothing, t) -> t
    (Just subtree, tWithout) -> insertSubtreeAt dest subtree tWithout

extractFolder :: [String] -> Tree -> (Maybe Tree, Tree)
extractFolder [] t = (Nothing, t)
extractFolder [seg] (Tree n fs f) =
  case break ((== seg) . tName) fs of
    (_, []) -> (Nothing, Tree n fs f)
    (before, matched : after) -> (Just matched, Tree n (before ++ after) f)
extractFolder (seg : segs) (Tree n fs f) =
  case break ((== seg) . tName) fs of
    (_, []) -> (Nothing, Tree n fs f)
    (before, matched : after) ->
      let (found, matched') = extractFolder segs matched
       in (found, Tree n (before ++ matched' : after) f)

insertSubtreeAt :: [String] -> Tree -> Tree -> Tree
insertSubtreeAt [] subtree (Tree n fs f) = Tree n (fs ++ [subtree]) f
insertSubtreeAt (seg : segs) subtree (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      child' = insertSubtreeAt segs subtree child
      fs' = child' : rest
   in Tree n fs' f

moveFile :: [String] -> [String] -> String -> Tree -> Tree
moveFile from to fname t =
  let (t', removed) = removeFileAt from fname t
   in if removed then insertFileAt to fname t' else t'

removeFileAt :: [String] -> String -> Tree -> (Tree, Bool)
removeFileAt [] fname (Tree n fs f) =
  if fname `elem` f then (Tree n fs (filter (/= fname) f), True) else (Tree n fs f, False)
removeFileAt (seg : segs) fname (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      (child', removed) = removeFileAt segs fname child
      fs' = child' : rest
   in (Tree n fs' f, removed)

applyToTree :: Tree -> Lib1.Command -> Tree
applyToTree t cmd = case cmd of
  Lib1.AddFolder path nameStr ->
    let segs = pathToSegments path
        newName = showAlphanumStr nameStr
     in insertFolderAt segs newName t
  Lib1.AddFile path (Lib1.File name dat) ->
    let segs = pathToSegments path
        fname = showName name ++ "#" ++ showData dat -- include data here
     in insertFileAt segs fname t
  Lib1.DeleteFolder path ->
    let segs = pathToSegments path
     in removeFolderAt segs t
  Lib1.MoveFolder from to ->
    let s = pathToSegments from
        d = pathToSegments to
     in moveFolder s d t
  Lib1.MoveFile from to name ->
    let s = pathToSegments from
        d = pathToSegments to
        fname = showName name
     in moveFile s d fname t
  Lib1.DeleteFile from name ->
    let s = pathToSegments from
        fname = showName name
     in fst $ removeFileAt s fname t
  _ -> t

-- Build file system from commands
buildTree :: [Lib1.Command] -> Tree
buildTree = foldl applyToTree emptyTree

formattedTreeOtp :: Tree -> [String]
formattedTreeOtp = go 0
  where
    indent n = replicate (2 * n) ' '
    go lvl (Tree name fs files) =
      let prefix = if null name then "" else indent lvl ++ name ++ " -> ["
          folderLines = concatMap (go (lvl + 1)) fs
          fileLine =
            ([indent (lvl + 1) ++ concatMap (++ "\t") files & init | not (null files)]) -- remove last tab
          closing = ([indent lvl ++ "]" | not (null name)])
       in ([prefix | not (null name)]) ++ folderLines ++ fileLine ++ closing

(&) :: b -> (b -> c) -> c
(&) = flip ($)

-- Dump, DumpHierarchy or unknown do not change tree

-- | Business/domain logic happens here.
-- This function makes your program actually usefull.
-- You may print if you want to print, you
-- may mutate state if needed but there must be
-- SINGLE atomically call in the function
-- You do not want to write/read files here.
{-# NOINLINE globalChan #-}
globalChan :: Chan StorageOp
globalChan = unsafePerformIO newChan

execute :: TVar State -> Lib1.Command -> IO ()
execute tvar cmd = case cmd of
  Lib1.DumpHierarchy -> do
    State cmds <- readTVarIO tvar
    let chronological = reverse cmds
        tree = buildTree chronological
    mapM_ putStrLn (formattedTreeOtp tree)
  _ -> do
    atomically $ do
      s <- readTVar tvar
      let s' = computeNextState s cmd
      writeTVar tvar s'
    rewriteStateFile tvar globalChan

-- Generate optimized command list from the current filesystem
optimizeCommands :: Tree -> [Lib1.Command]
optimizeCommands (Tree "" fs _f) = concatMap genFolder fs
  where
    genFolder :: Tree -> [Lib1.Command]
    genFolder t@(Tree name subFs _files) =
      let path = Lib1.stringToPath name
          createThis = [Lib1.AddFolder path (Lib1.stringToAlphanumStr name)]
       in -- For now we ignore subfolders/files; just handle first level
          createThis

rewriteStateFile :: TVar State -> Chan StorageOp -> IO ()
rewriteStateFile tvar ch = do
  State cmds <- readTVarIO tvar
  let tree = buildTree (reverse cmds)
      optimized = optimizeCommands tree
      payload = unlines $ map Lib2.toCliCommand optimized
  ack <- newChan
  writeChan ch (Save payload ack)
  _ <- readChan ack
  return ()

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request. It must run forever.
-- Modify as needed.
data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = do
  op <- readChan c
  case op of
    Save content ack -> do
      _res <- try (writeFile fileName content) :: IO (Either SomeException ())
      case _res of
        Left _ -> writeChan ack ()
        Right () -> writeChan ack ()
      storageOpLoop c
    Load resp -> do
      eres <- try (readFile fileName) :: IO (Either SomeException String)
      case eres of
        Left _ -> writeChan resp ""
        Right txt -> writeChan resp txt
      storageOpLoop c

stateToText :: State -> String
stateToText (State cmds) = unlines $ map Lib2.toCliCommand (reverse cmds)

-- | This function will be called periodically
-- and on programs' exit. File writes must be performed
-- through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save ch tvar = do
  s <- readTVarIO tvar
  let payload = stateToText s
  ack <- newChan
  writeChan ch (Save payload ack)
  _ <- readChan ack
  return $ Right ()

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load ch tvar = do
  resp <- newChan
  writeChan ch (Load resp)
  content <- readChan resp
  let ls = filter (not . null) (lines content)
      parsedOrErrs = map (runParser parseCommand) ls
  case sequence parsedOrErrs of
    Left e -> return $ Left e
    Right pairs ->
      let cmds = map fst pairs
          finalState = foldl computeNextState emptyState cmds
       in atomically (writeTVar tvar finalState) >> return (Right ())
