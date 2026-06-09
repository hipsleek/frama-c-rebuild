Compilation of the log_truncation.ml file.
  $ dune build --cache=disabled --root . _build/default/log_truncation.cmxs

  $ frama-c -no-autoload-plugins -load-module log_truncation.cmxs > file-log.txt

The message should not be truncated as the output was redireted to a file.
  $ cat file-log.txt | tr -d '#'
  [kernel] Warning: This is a very, very long message intended to test the truncation of the Frama-C log: 
    
    begin_line <----------> end_line
    [ This text should only appear in the log if is is redirected to a file. ]
    begin_line <----------> end_line

  $ socat EXEC:"frama-c -no-autoload-plugins -load-module log_truncation.cmxs",pty GOPEN:terminal-log.txt

The message should be truncated.
  $ cat terminal-log.txt | tr -d '#'
  [kernel] Warning: (truncated message) This is a very, very long message intended to test the truncation of the Frama-C log: 
    
    begin_line <-----[...]-----> end_line
