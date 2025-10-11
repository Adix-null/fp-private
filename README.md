BNF for the filesystem

<file> ::= <name> "#" <data>
<folder> ::= <alphanumstr> " -> [\n" (<filerec> | <folderrec>) "\n]\n"

<folderrec> ::= <folder> | <folderrec> <folder>
<filerec> ::= "null" | <file> | <filerec> "\t" <file>

<name> ::= <alphanumstr> "." <extension>
<alphanumstr> ::= <azAZ09> | <azAZ09> <alphanumstr>
<extension> ::= "txt" | "png" | "jpg" | "json" | "dat" | "exe" | "hs" | "cs" | "html" | "cpp" | "mp4" | "mp3"
<azAZ09> ::= [a-z] | [A-Z] | [0-9]
<symbols> ::=  "\t" | "!" | "\"" | "$" | "%" | "&" | "'" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | "<" | "=" | ">" | "?" | "@" | "\\" | "^" | "_" | "`" | "{" | "|" | "}" | "~" 
<ascii> ::= <azAZ09> | <symbols>
<data> ::= <ascii> | <data> <ascii>

<command> ::= <AddFile> | <DeleteFile> | <AddFolder> | <DeleteFolder> | <MoveFolder> | <MoveFile> | <PrintFS>
<path> ::= <alphanumstr> | <alphanumstr> "/" <path>
<AddFile> ::= "AddFile " <path> " " <file>
<DeleteFile> ::= "DeleteFile " <path> " " <name>
<AddFolder> ::= "AddFolder " <path> " " <alphanumstr>
<DeleteFolder> ::= "DeleteFolder " <path>
<MoveFolder> ::= "MoveFolder " <path> " " <path>
<MoveFile> ::= "MoveFile " <path> " " <path> " " <name>
<PrintFS> ::= "PrintFS "