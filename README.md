## State persistence
My structure is a filesystem, so the only 2 (3) commands necessary for creating the state are AddFile and AddFolder (Also AddFolderAtRoot because AddFolder takes 2 args). \
The in memory state of the filesystem is read, and the needed command list is generated, it's not just a history of cli inputs. \
I've also created command PrintFS that prints the filesystem in a tree visualization, so for example this cmd list

- AddFolderAtRoot f1
- AddFolder f1 f2
- AddFolder f1/f2 f3
- AddFile f1/f2/f3 mp.exe#123$%
- AddFolder f1/f2 w
- AddFolderAtRoot g

generates this in memory structure (visualized in console)

```
  f1 -> [
    f2 -> [
      f3 -> [
        mp.exe#123$%
      ]
      w -> [
      ]
    ]
  ]
  g -> [
  ]
  ```
Another example:
- AddFolderAtRoot z
- AddFolder z y
- AddFile z/y md.jpg#@&*123
- AddFolderAtRoot a
- AddFolder a b
- AddFolder a/b c
- AddFile a a.cs#dtnt
```
  z -> [
    y -> [
      md.jpg#@&*123
    ]
  ]
  a -> [
    b -> [
      c -> [
      ]
    ]
    a.cs#dtnt
  ]
```
### Screenshots of demo:

1. Program launched
<img width="682" height="1179" alt="s1" src="https://github.com/user-attachments/assets/63afc9eb-b1ce-47d6-9cb0-fb1b31b303f6" />

2. Root folder added 
<img width="731" height="1090" alt="s2" src="https://github.com/user-attachments/assets/f43e4442-f659-4026-8c9d-aedd1f7c163c" />

3. Two more folders added
<img width="648" height="1092" alt="s3" src="https://github.com/user-attachments/assets/08c6c930-1396-4e5c-ba7d-b365f4bd24e3" />

4. File added
<img width="734" height="1126" alt="s4" src="https://github.com/user-attachments/assets/a448e19c-d9bb-4f4f-9f20-57ccbb42303b" />

5. f folder deleted
<img width="702" height="1169" alt="s5" src="https://github.com/user-attachments/assets/218e946f-3d89-40f4-8002-4dfe47d88421" />

6. Filesystem printed for visualization
<img width="692" height="1280" alt="s6" src="https://github.com/user-attachments/assets/5946642c-8305-4fff-8907-4c15882f9bad" />

7. Program restarted and state printed 
<img width="665" height="1290" alt="s7" src="https://github.com/user-attachments/assets/8e4abad1-55b9-465b-9ef9-85c9665928ef" />

## BNF for the filesystem


https://bnfplayground.pauliankline.com/?bnf=%3CFS%3E%20%3A%3A%3D%20%3Cfilerec%3E%20%7C%20%3Cfolderrec%3E%0A%3Cfile%3E%20%3A%3A%3D%20%3Cname%3E%20%22%23%22%20%3Cdata%3E%0A%3Cfolder%3E%20%3A%3A%3D%20%3Calphanumstr%3E%20%22%20-%3E%20%5B%5Cn%22%20%3CFS%3E%20%22%5Cn%5D%5Cn%22%0A%0A%3Cfolderrec%3E%20%3A%3A%3D%20%3Cfolder%3E%20%7C%20%3Cfolderrec%3E%20%3Cfolder%3E%0A%3Cfilerec%3E%20%3A%3A%3D%20%22null%22%20%7C%20%3Cfile%3E%20%7C%20%3Cfilerec2%3E%20%22%5Ct%22%20%3Cfile%3E%0A%3Cfilerec2%3E%20%3A%3A%3D%20%3Cfile%3E%20%7C%20%3Cfilerec2%3E%20%22%5Ct%22%20%3Cfile%3E%0A%0A%3Cname%3E%20%3A%3A%3D%20%3Calphanumstr%3E%20%22.%22%20%3Cextension%3E%0A%3Calphanumstr%3E%20%3A%3A%3D%20%3CazAZ09%3E%20%7C%20%3CazAZ09%3E%20%3Calphanumstr%3E%0A%3CazAZ09%3E%20%3A%3A%3D%20%5Ba-z%5D%20%7C%20%5BA-Z%5D%20%7C%20%5B0-9%5D%0A%3Cextension%3E%20%3A%3A%3D%20%22txt%22%20%7C%20%22png%22%20%7C%20%22jpg%22%20%7C%20%22json%22%20%7C%20%22dat%22%20%7C%20%22exe%22%20%7C%20%22hs%22%20%7C%20%22cs%22%20%7C%20%22html%22%20%7C%20%22cpp%22%20%7C%20%22mp4%22%20%7C%20%22mp3%22%0A%3Csymbol%3E%20%3A%3A%3D%20%20%22!%22%20%7C%20%22%5C%22%22%20%7C%20%22%24%22%20%7C%20%22%25%22%20%7C%20%22%26%22%20%7C%20%22%27%22%20%7C%20%22(%22%20%7C%20%22)%22%20%7C%20%22*%22%20%7C%20%22%2B%22%20%7C%20%22%2C%22%20%7C%20%22-%22%20%7C%20%22.%22%20%7C%20%22%3A%22%20%7C%20%22%3B%22%20%7C%20%22%3C%22%20%7C%20%22%3D%22%20%7C%20%22%3E%22%20%7C%20%22%3F%22%20%7C%20%22%40%22%20%7C%20%22%5C%5C%22%20%7C%20%22%5E%22%20%7C%20%22_%22%20%7C%20%22%60%22%20%7C%20%22%7B%22%20%7C%20%22%7C%22%20%7C%20%22%7D%22%20%7C%20%22~%22%0A%3Cascii%3E%20%3A%3A%3D%20%3CazAZ09%3E%20%7C%20%3Csymbol%3E%0A%3Cdata%3E%20%3A%3A%3D%20%3Cascii%3E%20%7C%20%3Cdata%3E%20%3Cascii%3E%0A%0A%3Cpath%3E%20%3A%3A%3D%20%3Calphanumstr%3E%20%7C%20%3Calphanumstr%3E%20%22%2F%22%20%3Cpath%3E%0A%3Ccommand%3E%20%3A%3A%3D%20%3CDump%3E%20%7C%20%3CAddFile%3E%20%7C%20%3CDeleteFile%3E%20%7C%20%3CAddFolder%3E%20%7C%20%3CDeleteFolder%3E%20%7C%20%3CMoveFolder%3E%20%7C%20%3CMoveFile%3E%0A%3CDump%3E%20%3A%3A%3D%20%22Dump%20%22%20%22Examples%22%0A%3CAddFile%3E%20%3A%3A%3D%20%22AddFile%20%22%20%3Cpath%3E%20%22%20%22%20%3Cfile%3E%0A%3CMoveFile%3E%20%3A%3A%3D%20%22MoveFile%20%22%20%3Cpath%3E%20%22%20%22%20%3Cpath%3E%20%22%20%22%20%3Cname%3E%0A%3CDeleteFile%3E%20%3A%3A%3D%20%22DeleteFile%20%22%20%3Cpath%3E%20%22%20%22%20%3Cname%3E%0A%3CAddFolder%3E%20%3A%3A%3D%20%22AddFolder%20%22%20%3Cpath%3E%20%22%20%22%20%3Calphanumstr%3E%0A%3CMoveFolder%3E%20%3A%3A%3D%20%22MoveFolder%20%22%20%3Cpath%3E%20%22%20%22%20%3Cpath%3E%0A%3CDeleteFolder%3E%20%3A%3A%3D%20%22DeleteFolder%20%22%20%3Cpath%3E%0A%3CAddFolderAtRoot%3E%20%3A%3A%3D%20%22AddFolderAtRoot%20%22%20%3CalphanumStr%3E%0A&name=
