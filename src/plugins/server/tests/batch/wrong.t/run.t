  $ frama-c -no-autoload-plugins -load-plugin server wrong.i -server-batch wrong.json -server-msg-key use-relative-filepath
  [kernel] Parsing wrong.i (no preprocessing)
  [server] Script "wrong.json"
  [server] User Error: [batch] "unknown request": request "kernel.unknown" not found
  [server] User Error: [batch] "wrong data": request "kernel.ast.printFunction" not found
  [server] Output "wrong.out.json"
  [server] Deferred error message was emitted during execution:
    [batch] "unknown request": request "kernel.unknown" not found
  [kernel] Plug-in server aborted: invalid user input.
  [1]
  $ cat wrong.out.json
  [
    { "id": "unknown request", "error": "request not found" },
    { "id": "wrong data", "error": "request not found" }
  ]
