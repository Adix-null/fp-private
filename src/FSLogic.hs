{-# LANGUAGE InstanceSigs #-}
module FSLogic where

import Lib1
    ( ASCII(..),
      AlphanumStr(..),
      AzAZ09(..),
      Command(..),
      Data(..),
      File(File),
      Name(..),
      Path(..),
      Symbol(..) )
import Lib4 ( Parser, parseCommand, Input, ErrorMsg ) 
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runState)

-- boilerplate converters
dataToString :: Lib1.Data -> String
dataToString (Lib1.SingleASCII a) = [asciiToChar a]
dataToString (Lib1.RecASCII a rest) = asciiToChar a : dataToString rest

asciiToChar :: Lib1.ASCII -> Char
asciiToChar (Lib1.Alphanum a) = azAZ09ToChar a
asciiToChar (Lib1.Symbol s) = symbolToChar s

azAZ09ToChar :: Lib1.AzAZ09 -> Char
azAZ09ToChar a = case a of
  Lib1.Lower c -> c
  Lib1.Upper c -> c
  Lib1.Digit c -> c

symbolToChar :: Lib1.Symbol -> Char
symbolToChar s = case s of
  Lib1.SymExclam -> '!'
  Lib1.SymQuote -> '"'
  Lib1.SymDollar -> '$'
  Lib1.SymPercent -> '%'
  Lib1.SymAmpersand -> '&'
  Lib1.SymApostrophe -> '\''
  Lib1.SymLParen -> '('
  Lib1.SymRParen -> ')'
  Lib1.SymAsterisk -> '*'
  Lib1.SymPlus -> '+'
  Lib1.SymComma -> ','
  Lib1.SymMinus -> '-'
  Lib1.SymDot -> '.'
  Lib1.SymColon -> ':'
  Lib1.SymSemicolon -> ';'
  Lib1.SymLt -> '<'
  Lib1.SymEq -> '='
  Lib1.SymGt -> '>'
  Lib1.SymQMark -> '?'
  Lib1.SymAt -> '@'
  Lib1.SymBackslash -> '\\'
  Lib1.SymCaret -> '^'
  Lib1.SymUnderscore -> '_'
  Lib1.SymBacktick -> '`'
  Lib1.SymLCurly -> '{'
  Lib1.SymPipe -> '|'
  Lib1.SymRCurly -> '}'
  Lib1.SymTilde -> '~'

-- helper show functions
showName :: Lib1.Name -> String
showName (Lib1.Name an ext) = showAlphanumStr an ++ "." ++ show ext

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


runParser :: Parser a -> Input -> Either ErrorMsg a
runParser p input =
  case runState (runExceptT p) input of
    (Left e, _) -> Left e
    (Right v, _) -> Right v

newtype FSState = FSState
  { unFS :: [Command]
  }
  deriving (Show)

instance Eq FSState where
  (==) :: FSState -> FSState -> Bool
  (FSState cmds1) == (FSState cmds2) = cmds1 == cmds2


data Tree = Tree
  { tName :: String,
    tFolders :: [Tree],
    tFiles :: [(String, Lib1.Data)]
  }
  deriving (Show, Eq)

emptyFS :: FSState
emptyFS = FSState []

emptyTree :: Tree
emptyTree = Tree "" [] []

buildTree :: [Lib1.Command] -> Tree
buildTree = foldl applyToTree emptyTree

-- functions to logically change state
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

insertFolderAtRoot :: String -> Tree -> Tree
insertFolderAtRoot newName (Tree n fs f) =
  if any ((== newName) . tName) fs
    then Tree n fs f
    else Tree n (fs ++ [Tree newName [] []]) f

insertFileAt :: [String] -> (String, Lib1.Data) -> Tree -> Tree
insertFileAt [] fnameWithData@(fname, _) (Tree n fs f) =
  if any ((== fname) . fst) f then Tree n fs f else Tree n fs (f ++ [fnameWithData])
insertFileAt (seg : segs) fnameWithData (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      child' = insertFileAt segs fnameWithData child
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
    (Just subtree, tWithout) -> insertSubfolderAt dest subtree tWithout

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

insertSubfolderAt :: [String] -> Tree -> Tree -> Tree
insertSubfolderAt [] subtree (Tree n fs f) = Tree n (fs ++ [subtree]) f
insertSubfolderAt (seg : segs) subtree (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      child' = insertSubfolderAt segs subtree child
      fs' = child' : rest
   in Tree n fs' f

moveFile :: [String] -> [String] -> String -> Tree -> Tree
moveFile from to fname t =
  let (t', removed, fdata) = removeFileAtWithData from fname t
   in (if removed then insertFileAt to (fname, fdata) t' else t')

removeFileAtWithData :: [String] -> String -> Tree -> (Tree, Bool, Lib1.Data)
removeFileAtWithData [] fname (Tree n fs f) =
  case lookup fname f of
    Just dat -> (Tree n fs (filter ((/= fname) . fst) f), True, dat)
    Nothing -> (Tree n fs f, False, Lib1.SingleASCII (Lib1.Alphanum (Lib1.Lower ' ')))
removeFileAtWithData (seg : segs) fname (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      (child', removed, dat) = removeFileAtWithData segs fname child
   in (Tree n (child' : rest) f, removed, dat)

removeFileAt :: [String] -> String -> String -> Tree -> (Tree, Bool)
removeFileAt [] base ext (Tree n fs f) =
  let fullName = base ++ "." ++ ext
      filtered = filter (\(fname, _) -> fname /= fullName) f
      removed = length filtered < length f
   in (Tree n fs filtered, removed)
removeFileAt (seg : segs) base ext (Tree n fs f) =
  let (child, rest) = findOrCreate seg fs
      (child', removed) = removeFileAt segs base ext child
      fs' = child' : rest
   in (Tree n fs' f, removed)

applyToTree :: Tree -> Lib1.Command -> Tree
applyToTree t cmd = case cmd of
  Lib1.AddFile path (Lib1.File name dat) -> insertFileAt (pathToSegments path) (showName name, dat) t
  Lib1.MoveFile from to (Lib1.Name base ext) -> moveFile (pathToSegments from) (pathToSegments to) (showAlphanumStr base ++ "." ++ show ext) t
  Lib1.DeleteFile from (Lib1.Name base ext) -> fst $ removeFileAt (pathToSegments from) (showAlphanumStr base) ("." ++ show ext) t
  Lib1.AddFolder path nameStr -> insertFolderAt (pathToSegments path) (showAlphanumStr nameStr) t
  Lib1.MoveFolder from to -> moveFolder (pathToSegments from) (pathToSegments to) t
  Lib1.DeleteFolder path -> removeFolderAt (pathToSegments path) t
  Lib1.AddFolderAtRoot nameStr -> insertFolderAtRoot (showAlphanumStr nameStr) t
  _ -> t

commandKeys :: Lib1.Command -> [String]
commandKeys cmd = case cmd of
  Lib1.AddFile path (Lib1.File (Lib1.Name an _ext) _dat) -> ["file:" ++ showPath path ++ "/" ++ showAlphanumStr an]
  Lib1.MoveFile from to (Lib1.Name an _ext) -> ["file:" ++ showPath from ++ "/" ++ showAlphanumStr an, "file:" ++ showPath to ++ "/" ++ showAlphanumStr an]
  Lib1.DeleteFile path (Lib1.Name an _ext) -> ["file:" ++ showPath path ++ "/" ++ showAlphanumStr an]
  Lib1.AddFolder path nameStr -> ["folder:" ++ showPath path ++ "/" ++ showAlphanumStr nameStr]
  Lib1.MoveFolder from to -> ["folder:" ++ showPath from, "folder:" ++ showPath to]
  Lib1.DeleteFolder path -> ["folder:" ++ showPath path]
  Lib1.AddFolderAtRoot nameStr -> ["folder:/" ++ showAlphanumStr nameStr]
  Lib1.Dump _ -> ["dump"]
  Lib1.PrintFS -> ["PrintFS"]

(&) :: b -> (b -> c) -> c
(&) = flip ($)

fsToRecipe :: FSState -> String
fsToRecipe (FSState cmds) =
  let tree = buildTree (reverse cmds)
   in unlines $ concatMap goRoot (tFolders tree)
  where
    goRoot :: Tree -> [String]
    goRoot (Tree name fs files) =
      let rootCmd = ["AddFolderAtRoot " ++ name]
          fileCmds = map (\(fname, dat) -> "AddFile " ++ name ++ " " ++ fname ++ "#" ++ dataToString dat) files
          subfolderCmds = concatMap (goSub name) fs
       in rootCmd ++ subfolderCmds ++ fileCmds

    goSub :: String -> Tree -> [String]
    goSub path (Tree name fs files) =
      let currentPath = path ++ "/" ++ name
          folderCmd = ["AddFolder " ++ path ++ " " ++ name]
          fileCmds = map (\(fname, fdata) -> "AddFile " ++ currentPath ++ " " ++ fname ++ "#" ++ dataToString fdata) files
          subfolderCmds = concatMap (goSub currentPath) fs
       in folderCmd ++ subfolderCmds ++ fileCmds


-- Format tree like in bnf
formattedTreeOtp :: Tree -> [String]
formattedTreeOtp = go 0
  where
    indent n = replicate (2 * n) ' '
    go lvl (Tree name fs files) =
      let prefix = if null name then "" else indent lvl ++ name ++ " -> ["
          folderLines = concatMap (go (lvl + 1)) fs
          fileLine =
            ([indent (lvl + 1) ++ concatMap (\(fname, dat) -> fname ++ "#" ++ dataToString dat ++ "\t") files & init | not (null files)])
          closing = [indent lvl ++ "]" | not (null name)]
       in ([prefix | not (null name)]) ++ folderLines ++ fileLine ++ closing


computeNextState :: FSState -> Lib1.Command -> FSState
computeNextState (FSState old) cmd =
  let keys = commandKeys cmd
      keep c = null (keys `intersect` commandKeys c)
      newList = cmd : filter keep old
   in FSState newList
  where
    intersect a b = [x | x <- a, x `elem` b]

saveState :: FilePath -> FSState -> IO ()
saveState path st =
  writeFile path (fsToRecipe st)

loadState :: FilePath -> IO FSState
loadState path = do
    txt <- readFile path
    let ls = filter (not . null) (lines txt)
    case traverse (runParser parseCommand) ls of
        Left _ -> return emptyFS
        Right cmds -> return $ FSState cmds