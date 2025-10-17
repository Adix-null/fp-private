-- module Lib1
--   ( examples,
--     Command (..),
--     Dumpable (..),
--     File (..),
--   )
-- where

import GHC.Generics (D)

data Dumpable = Examples
  deriving (Show)

-- <FS> ::= <filerec> | <folderrec>
data FS
  = FileRec FileRec
  | FolderRec FolderRec
  deriving (Show, Eq)

-- <file> ::= <name> "#" <data>
data File
  = File Name Data
  deriving (Show, Eq)

-- <folder> ::= <alphanumstr> " -> [\n" <FS> "\n]\n"
data Folder
  = Folder AlphanumStr FS
  deriving (Show, Eq)

-- <folderrec> ::= <folder> | <folderrec> <folder>
data FolderRec
  = SingleFolder Folder
  | RecFolder Folder FolderRec
  deriving (Show, Eq)

-- <filerec> ::= "null" | <file> | <filerec2> "\t" <file>
data FileRec
  = NullFile
  | SingleFile File
  | RecFile FileRec2 File
  deriving (Show, Eq)

-- <filerec2> ::= <file> | <filerec2> "\t" <file>
data FileRec2
  = SingleFile2 File
  | RecFile2 FileRec2 File
  deriving (Show, Eq)

-- <name> ::= <alphanumstr> "." <extension>
data Name
  = Name AlphanumStr Extension
  deriving (Show, Eq)

-- <alphanumstr> ::= <azAZ09> | <azAZ09> <alphanumstr>
data AlphanumStr
  = Single AzAZ09
  | Rec AzAZ09 AlphanumStr
  deriving (Show, Eq)

-- <azAZ09> ::= [a-z] | [A-Z] | [0-9]
data AzAZ09
  = Lower Char
  | Upper Char
  | Digit Char
  deriving (Show, Eq)

-- <extension> ::= "txt" | "png" | "jpg" | "json" | "dat" | "exe" | "hs" | "cs" | "html" | "cpp" | "mp4" | "mp3"
data Extension
  = Txt
  | Png
  | Jpg
  | Json
  | Dat
  | Exe
  | Hs
  | Cs
  | Html
  | Cpp
  | Mp4
  | Mp3
  deriving (Show, Eq)

-- <symbol> ::=  "\t" | "!" | "\"" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," | "-" | "." | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "\\" | "^" | "_" | "`" | "{" | "|" | "}" | "~"
data Symbol
  = SymTab
  | SymExclam
  | SymQuote
  | SymDollar
  | SymPercent
  | SymAmpersand
  | SymApostrophe
  | SymLParen
  | SymRParen
  | SymAsterisk
  | SymPlus
  | SymComma
  | SymMinus
  | SymDot
  | SymColon
  | SymSemicolon
  | SymLt
  | SymEq
  | SymGt
  | SymQMark
  | SymAt
  | SymBackslash
  | SymCaret
  | SymUnderscore
  | SymBacktick
  | SymLCurly
  | SymPipe
  | SymRCurly
  | SymTilde
  deriving (Show, Eq)

-- <ascii> ::= <azAZ09> | <symbols>
data ASCII
  = ASCII AzAZ09 Symbol
  deriving (Show, Eq)

-- <data> ::= <ascii> | <data> <ascii>
data Data
  = SingleASCII ASCII
  | RecASCII ASCII Data
  deriving (Show, Eq)

-- <path> ::= <alphanumstr> | <alphanumstr> "/" <path>
data Path
  = SinglePath AlphanumStr
  | RecPath AlphanumStr Path
  deriving (Show, Eq)

-- <command> ::= <AddFile> | <MoveFile> | <DeleteFile> | <AddFolder> | <MoveFolder> | <DeleteFolder>
data Command
  = Dump Dumpable
  | AddFile AddFile
  | MoveFile MoveFile
  | DeleteFile DeleteFile
  | AddFolder AddFolder
  | MoveFolder MoveFolder
  | DeleteFolder DeleteFolder
  deriving (Show)

-- <AddFile> ::= "AddFile " <path> " " <file>
data AddFile
  = AddFileC Path File
  deriving (Show, Eq)

-- <DeleteFile> ::= "DeleteFile " <path> " " <name>
data DeleteFile
  = DeleteFileC Path Name
  deriving (Show, Eq)

-- <AddFolder> ::= "AddFolder " <path> " " <alphanumstr>
data AddFolder
  = AddFolderC Path AlphanumStr
  deriving (Show, Eq)

-- <DeleteFolder> ::= "DeleteFolder " <path>
data DeleteFolder
  = DeleteFolderC Path
  deriving (Show, Eq)

-- <MoveFolder> ::= "MoveFolder " <path> " " <path>
data MoveFolder
  = MoveFolderC Path Path
  deriving (Show, Eq)

-- <MoveFile> ::= "MoveFile " <path> " " <path> " " <name>
data MoveFile
  = MoveFileC Path Path Name
  deriving (Show, Eq)

-- stringToAlphanumStr :: String -> AlphanumStr
-- stringToAlphanumStr [c] = Single c
-- stringToAlphanumStr (c : cs) = Rec c (stringToAlphanumStr cs)
-- stringToAlphanumStr [] = error "empty alphanum string"

-- stringToPath :: String -> Path
-- stringToPath s =
--   let parts = splitOn '/' s
--    in buildPath parts
--   where
--     buildPath :: [String] -> Path
--     buildPath [p] = SinglePath (stringToAlphanumStr p)
--     buildPath (p : ps) = RecPath (stringToAlphanumStr p) (buildPath ps)
--     buildPath [] = error "empty path"

-- -- simple split function
-- splitOn :: Char -> String -> [String]
-- splitOn _ [] = [""]
-- splitOn delim (c : cs)
--   | c == delim = "" : rest
--   | otherwise = (c : head rest) : tail rest
--   where
--     rest = splitOn delim cs

-- --Example commands
-- examples :: [Command]
-- examples =
--   [ Dump Examples,
--     -- AddFileC "a/b/c" "WS.exe#Px",
--     -- MoveFile "0/YC" "RV/c9" "3.html",
--     -- DeleteFile "i" "3m.txt",
--     -- AddFolder "x/y/z" "GM",
--     -- MoveFolder "p/a/t/h5" "f1/f2",
--     DeleteFolder (DeleteFolder (RecPath (AlphanumStr  "pt") (SinglePath (AlphanumStr "6q"))))
--   ]
