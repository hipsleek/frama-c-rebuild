  $ dune exec --cache=disabled -- frama-c -no-autoload-plugins -load-plugin server ast_services.i -server-batch ast_services.json -server-msg-key use-relative-filepath
  [kernel] Parsing ast_services.i (no preprocessing)
  [server] Script "ast_services.json"
  [server] [GET] kernel.ast.fetchFunctions
  [server] [GET] kernel.ast.fetchFunctions
  [server] Output "ast_services.out.json"
  $ cat ast_services.out.json
  [
    {
      "id": "GET-1",
      "data": {
        "updated": [
          {
            "key": "kf#25",
            "decl": "#F25",
            "name": "g",
            "signature": "int g(int y);",
            "defined": true,
            "sloc": {
              "dir": ".",
              "base": "ast_services.i",
              "file": "ast_services.i",
              "line": 2
            },
            "filters": [
              [ "builtin", false ],
              [ "stdlib", false ],
              [ "defined", true ],
              [ "extern", false ],
              [ "ghost", false ]
            ]
          },
          {
            "key": "kf#21",
            "decl": "#F21",
            "name": "f",
            "signature": "int f(int x);",
            "defined": true,
            "sloc": {
              "dir": ".",
              "base": "ast_services.i",
              "file": "ast_services.i",
              "line": 1
            },
            "filters": [
              [ "builtin", false ],
              [ "stdlib", false ],
              [ "defined", true ],
              [ "extern", false ],
              [ "ghost", false ]
            ]
          }
        ],
        "removed": [],
        "reload": true,
        "pending": 0
      }
    },
    {
      "id": "GET-2",
      "data": { "updated": [], "removed": [], "reload": false, "pending": 0 }
    }
  ]
