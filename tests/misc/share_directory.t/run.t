  $ dune build --cache=disabled  --root . @install

Basic case
  $ dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories
  [kernel] IS_SET false
  [dirs] path (dir)
  [dirs] Found: FRAMAC_SHARE/dirs/path
  [dirs] Path (.)
  [dirs] Found: FRAMAC_SHARE/dirs/path
  [dirs] path/file.txt (file)
  [dirs] Found: FRAMAC_SHARE/dirs/path/file.txt
  [dirs] Path (file)
  [dirs] Found: FRAMAC_SHARE/dirs/path/file.txt
  [dirs] foo (dir)
  [dirs] User Error: Could not find directory foo in Frama-C/Directories share
  [dirs] foo.txt (file)
  [dirs] User Error: Could not find file foo.txt in Frama-C/Directories share
  [dirs] path (file)
  [dirs] User Error: FRAMAC_SHARE/dirs/path is expected to be a file
  [dirs] path/file.txt
  [dirs] User Error: FRAMAC_SHARE/dirs/path/file.txt is expected to be a directory

With option
  $ cp -r share copied
  $ dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-module directories -dirs-share copied
  [kernel] IS_SET true
  [dirs] path (dir)
  [dirs] Found: copied/path
  [dirs] Path (.)
  [dirs] Found: copied/path
  [dirs] path/file.txt (file)
  [dirs] Found: copied/path/file.txt
  [dirs] Path (file)
  [dirs] Found: copied/path/file.txt
  [dirs] foo (dir)
  [dirs] User Error: Could not find directory foo in Frama-C/Directories share
  [dirs] foo.txt (file)
  [dirs] User Error: Could not find file foo.txt in Frama-C/Directories share
  [dirs] path (file)
  [dirs] User Error: copied/path is expected to be a file
  [dirs] path/file.txt
  [dirs] User Error: copied/path/file.txt is expected to be a directory
