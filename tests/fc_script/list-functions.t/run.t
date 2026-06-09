  $ frama-c-script list-functions find-fun2.c list-functions.c
  [kernel:typing:implicit-function-declaration] find-fun2.c:12: Warning: 
    Calling undeclared function false_positive. Old style K&R code?
  [kernel:typing:no-proto] find-fun2.c:21: Warning: 
    Function false_positive is declared without prototype.
    Its formals will be inferred from actual arguments at first call.
    Declare it as false_positive(void) if the function does not take any parameters.
  f: defined at find-fun2.c:6-8 (1 statement);
  g: defined at find-fun2.c:10-13 (3 statements);
  h: defined at find-fun2.c:15-17 (2 statements);
  k: defined at list-functions.c:13-18 (8 statements);

  $ frama-c-script list-functions find-fun2.c list-functions.c -list-functions-declarations -list-functions-output ./list-functions2.json -list-functions-debug 1
  [kernel:typing:implicit-function-declaration] find-fun2.c:12: Warning: 
    Calling undeclared function false_positive. Old style K&R code?
  [kernel:typing:no-proto] find-fun2.c:21: Warning: 
    Function false_positive is declared without prototype.
    Its formals will be inferred from actual arguments at first call.
    Declare it as false_positive(void) if the function does not take any parameters.
  [list-functions] List written to: list-functions2.json

  $ cat list-functions2.json
  [ { "extf": { "declarations": [ "list-functions2.h:1",
                                  "list-functions2.h:1" ] } },
    { "f": { "definitions": [ { "location": "find-fun2.c:6-8",
                                "statements": 1 } ] } },
    { "false_positive": { "declarations": [ "find-fun2.c:21" ] } },
    { "g": { "definitions": [ { "location": "find-fun2.c:10-13",
                                "statements": 3 } ] } },
    { "h": { "definitions": [ { "location": "find-fun2.c:15-17",
                                "statements": 2 } ] } },
    { "k": { "definitions": [ { "location": "list-functions.c:13-18",
                                "statements": 8 } ] } } ]
