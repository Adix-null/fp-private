{-# LANGUAGE InstanceSigs #-}
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
    extensions,
    stringToAlphanumStr,
    stringToPath,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

data Dumpable = Examples
  deriving (Show, Eq)

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
  deriving (Eq)

extensions :: [(String, Extension)]
extensions =
  [ ("txt", Txt),
    ("png", Png),
    ("jpg", Jpg),
    ("json", Json),
    ("dat", Dat),
    ("exe", Exe),
    ("hs", Hs),
    ("cs", Cs),
    ("html", Html),
    ("cpp", Cpp),
    ("mp4", Mp4),
    ("mp3", Mp3)
  ]

instance Show Extension where
  show ext = case ext of
    Txt -> "txt"
    Png -> "png"
    Jpg -> "jpg"
    Json -> "json"
    Dat -> "dat"
    Exe -> "exe"
    Hs -> "hs"
    Cs -> "cs"
    Html -> "html"
    Cpp -> "cpp"
    Mp4 -> "mp4"
    Mp3 -> "mp3"

-- <symbol> ::= "!" | "\"" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," | "-" | "." | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "\\" | "^" | "_" | "`" | "{" | "|" | "}" | "~"
data Symbol
  = SymExclam
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

-- <ascii> ::= <azAZ09> | <symbol>
data ASCII
  = Alphanum AzAZ09
  | Symbol Symbol
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
  | AddFile Path File -- <AddFile> ::= "AddFile " <path> " " <file>
  | MoveFile Path Path Name -- <MoveFile> ::= "MoveFile " <path> " " <path> " " <name>
  | DeleteFile Path Name -- <DeleteFile> ::= "DeleteFile " <path> " " <name>
  | AddFolder Path AlphanumStr -- <AddFolder> ::= "AddFolder " <path> " " <alphanumstr>
  | MoveFolder Path Path -- <MoveFolder> ::= "MoveFolder " <path> " " <path>
  | DeleteFolder Path -- <DeleteFolder> ::= "DeleteFolder " <path>
  | AddFolderAtRoot AlphanumStr -- <AddFolderAtRoot> ::= "AddFolderAtRoot " <AlphanumStr>
  | PrintFS
  deriving (Show)

instance Eq Lib1.Command where
  (==) :: Command -> Command -> Bool
  Lib1.AddFile pf1 d1 == Lib1.AddFile pf2 d2 = pf1 == pf2 && d1 == d2
  Lib1.MoveFile pf1 pt1 fln1 == Lib1.MoveFile pf2 pt2 fln2 = pf1 == pf2 && pt1 == pt2 && fln1 == fln2
  Lib1.DeleteFile pf1 fln1 == Lib1.DeleteFile pf2 fln2 = pf1 == pf2 && fln1 == fln2
  Lib1.AddFolder pf1 fdn1 == Lib1.AddFolder pf2 fdn2 = pf1 == pf2 && fdn1 == fdn2
  Lib1.MoveFolder pf1 pt1 == Lib1.MoveFolder pf2 pt2 = pf1 == pf2 && pt1 == pt2
  Lib1.DeleteFolder pf1 == Lib1.DeleteFolder pf2 = pf1 == pf2
  Lib1.AddFolderAtRoot fdn1 == Lib1.AddFolderAtRoot fdn2 = fdn1 == fdn2
  Lib1.Dump e1 == Lib1.Dump e2 = e1 == e2
  Lib1.PrintFS == Lib1.PrintFS = True
  _ == _ = False

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
    PrintFS,
    AddFile (RecPath (Single (Lower 'a')) (SinglePath (Rec (Lower 'b') (Single (Digit '1'))))) (File (Name (Rec (Upper 'W') (Single (Upper 'S'))) Exe) (RecASCII (Alphanum (Upper 'X')) (RecASCII (Symbol SymExclam) (SingleASCII (Alphanum (Lower 'p')))))),
    MoveFile (stringToPath "0/YC") (stringToPath "RV/c9") (Name (stringToAlphanumStr "3") Html),
    DeleteFile (stringToPath "path/from") (Name (stringToAlphanumStr "3m") Txt),
    AddFolder (stringToPath "x/y/z") (stringToAlphanumStr "Fold9Name"),
    MoveFolder (stringToPath "path/from") (stringToPath "f1/f2"),
    DeleteFolder (stringToPath "path/from"),
    AddFolderAtRoot (stringToAlphanumStr "root")
  ]
