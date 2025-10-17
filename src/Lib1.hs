module Lib1
  ( examples,
    Command (..),
    Dumpable (..),
    File (..),
  )
where

import GHC.Generics (D)

data Dumpable = Examples
  deriving (Show)

type AlphanumStr = String

type Name = String

type DataStr = String

type Path = String

data File = File
  { name :: Name,
    fileData :: DataStr
  }
  deriving (Show, Eq)

data Folder = Folder
  { folderName :: AlphanumStr,
    contents :: [FolderContent]
  }
  deriving (Show, Eq)

data FolderContent
  = FileRec File
  | FolderRec Folder
  deriving (Show, Eq)

data AzAZ09
  = Lower Char
  | Upper Char
  | Digit Char
  deriving (Show, Eq)

data Symbol
  = SymTab
  | SymExcl
  | SymQuote
  | SymDollar
  | SymPercent
  | SymAmp
  | SymApos
  | SymLParen
  | SymRParen
  | SymStar
  | SymPlus
  | SymComma
  | SymMinus
  | SymDot
  | SymSlash
  | SymColon
  | SymSemi
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

data Ascii
  = Aza AzAZ09
  | Sym Symbol
  deriving (Show, Eq)

data FileExt
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
  deriving (Show, Eq, Enum, Bounded)

-- Available commands
data Command
  = Dump Dumpable
  | AddFile Path String
  | MoveFile Path Path String
  | DeleteFile Path String
  | AddFolder Path AlphanumStr
  | MoveFolder Path Path
  | DeleteFolder Path
  deriving (Show)

-- Example commands
examples :: [Command]
examples =
  [ Dump Examples,
    AddFile "a/b/c" "WS.exe#Px",
    MoveFile "0/YC" "RV/c9" "3.html",
    DeleteFile "i" "3m.txt",
    AddFolder "x/y/z" "GM",
    MoveFolder "p/a/t/h5" "f1/f2",
    DeleteFolder "pt/6q"
  ]