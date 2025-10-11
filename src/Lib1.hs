module Lib1
  ( examples,
    Command (..),
    Dumpable (..),
  )
where

import GHC.Generics (D)

data Dumpable = Examples
  deriving (Show)

type AlphanumStr = String

type Name = String

type DataStr = String

type Extension = String

type Path = String

type ASCII = Char

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
  deriving (Show, Eq)

data Command
  = Dump Dumpable
  | AddFile Path File
  | DeleteFile Path Name
  | AddFolder Path AlphanumStr
  | DeleteFolder Path
  | MoveFolder Path Path
  | MoveFile Path Path Name
  | PrintFS
  deriving (Show)

apply :: Command -> Command -> Command
apply (Dump _) fs = fs

-- insertFile :: [String] -> String -> String -> String -> FS -> FS
-- insertFile _ _ _ _ fs = fs

-- deleteFile :: [String] -> String -> String -> FS -> FS
-- deleteFile _ _ _ fs = fs

insertFolder :: Folder -> Path -> AlphanumStr -> Either String Folder
insertFolder filesystem path newName =
  case splitPath path of
    [] -> Left "Empty path provided"
    [target] ->
      if folderName filesystem == target
        then
          Right
            ( filesystem
                { contents = contents filesystem ++ [FolderRec (Folder newName [])]
                }
            )
        else Left ("Folder not found: " ++ target)
    (p : ps) ->
      if folderName filesystem == p
        then
          let updateSub :: FolderContent -> Either String FolderContent
              updateSub (FolderRec f) =
                ( case insertFolder f (joinPath ps) newName of
                    Left err -> Left err
                    Right f' -> Right (FolderRec f')
                )
              updateSub other = Right other

              updatedContents = map updateSub (contents filesystem)
           in ( case sequence updatedContents of
                  Left err -> Left err
                  Right cs ->
                    Right
                      ( filesystem
                          { contents = cs
                          }
                      )
              )
        else Left ("Folder not found: " ++ p)

-- deleteFolder :: [String] -> FS -> FS
-- deleteFolder _ fs = fs

-- moveFile :: [String] -> [String] -> String -> String -> FS -> FS
-- moveFile _ _ _ _ fs = fs

-- moveFolder :: [String] -> [String] -> FS -> FS
-- moveFolder _ _ fs = fs

-- printFS :: FS -> FS
-- printFS fs = fs

-- helper functions
splitPath :: Path -> [String]
splitPath "" = []
splitPath p = case break (== '/') p of
  (seg, "") -> [seg]
  (seg, _ : rest) -> seg : splitPath rest

joinPath :: [String] -> Path
joinPath = foldr1 (\a b -> a ++ "/" ++ b)

examples :: [Command]
examples =
  [ Dump Examples,
    -- AddFile ["/"] "file1" "txt" "Data of file",
    -- DeleteFile ["/"] "file1" "txt",
    AddFolder "/" "folder1",
    -- DeleteFolder ["/", "folder1"],
    -- MoveFile ["/"] ["folder1"] "file1" "txt",
    -- MoveFolder ["/", "folder1"] ["/"],
    PrintFS
  ]
