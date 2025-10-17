module Lib1
  ( examples,
    Command (..),
    Dumpable (..),
    FS (..),
    File (..),
    Folder (..),
    FolderRec (..),
    FileRec (..),
    FileRec2 (..),
    Name (..),
    AlphanumStr (..),
    AzAZ09 (..),
    Extension (..),
    Symbol (..),
    ASCII (..),
    Data (..),
    Path (..),
    AddFile (..),
    MoveFile (..),
    DeleteFile (..),
    AddFolder (..),
    MoveFolder (..),
    DeleteFolder (..),
    stringToAlphanumStr,
    stringToPath,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
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

-- <MoveFile> ::= "MoveFile " <path> " " <path> " " <name>
data MoveFile
  = MoveFileC Path Path Name
  deriving (Show, Eq)

-- <DeleteFile> ::= "DeleteFile " <path> " " <name>
data DeleteFile
  = DeleteFileC Path Name
  deriving (Show, Eq)

-- <AddFolder> ::= "AddFolder " <path> " " <alphanumstr>
data AddFolder
  = AddFolderC Path AlphanumStr
  deriving (Show, Eq)

-- <MoveFolder> ::= "MoveFolder " <path> " " <path>
data MoveFolder
  = MoveFolderC Path Path
  deriving (Show, Eq)

-- <DeleteFolder> ::= "DeleteFolder " <path>
data DeleteFolder
  = DeleteFolderC Path
  deriving (Show, Eq)

stringToAlphanumStr :: String -> AlphanumStr
stringToAlphanumStr ch =
  case ch of
    [c] -> Single (charToAzAZ09 c)
    (c : cs) -> Rec (charToAzAZ09 c) (stringToAlphanumStr cs)
    [] -> error "empty string not allowed"
  where
    charToAzAZ09 :: Char -> AzAZ09
    charToAzAZ09 l =
      case l of
        _ | isAsciiLower l -> Lower l
        _ | isAsciiUpper l -> Upper l
        _ | isDigit l -> Digit l
        _ -> error $ "Invalid character for AlphanumStr: " ++ [l]

stringToPath :: String -> Path
stringToPath s =
  let splitArray = splitOn '/' s
   in buildPath splitArray
  where
    buildPath :: [String] -> Path
    buildPath [p] = SinglePath (stringToAlphanumStr p)
    buildPath (p : ps) = RecPath (stringToAlphanumStr p) (buildPath ps)
    buildPath [] = error "empty path"

    splitOn :: Char -> String -> [String]
    splitOn _ [] = [""]
    splitOn delim (c : cs)
      | c == delim = "" : rest
      | otherwise = (c : head rest) : tail rest
      where
        rest = splitOn delim cs

-- Example commands
examples :: [Command]
examples =
  [ Dump Examples,
    AddFile (AddFileC (stringToPath "a/b/c") (File (Name (stringToAlphanumStr "WS") Exe) (RecASCII (ASCII (Upper 'P') SymExclam) (SingleASCII (ASCII (Upper 'x') SymTab))))),
    MoveFile (MoveFileC (stringToPath "0/YC") (stringToPath "RV/c9") (Name (stringToAlphanumStr "3") Html)),
    DeleteFile (DeleteFileC (stringToPath "path/from") (Name (stringToAlphanumStr "3m") Txt)),
    AddFolder (AddFolderC (stringToPath "x/y/z") (stringToAlphanumStr "Fold9Name")),
    MoveFolder (MoveFolderC (stringToPath "path/from") (stringToPath "f1/f2")),
    DeleteFolder (DeleteFolderC $ stringToPath "path/from")
  ]
