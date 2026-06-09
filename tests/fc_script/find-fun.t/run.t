  $ frama-c-script find-fun main2 .
  Looking for 'main2' inside 10 file(s)...
  Possible declarations for function 'main2' in the following file(s):
    find-fun.c
  Possible definitions for function 'main2' in the following file(s):
    build-callgraph.c
    main2.c

  $ frama-c-script find-fun main3 .
  Looking for 'main3' inside 10 file(s)...
  Possible declarations for function 'main3' in the following file(s):
    find-fun2.c
  Possible definitions for function 'main3' in the following file(s):
    build-callgraph.c
    find-fun.c

  $ frama-c-script find-fun false_positive .
  Looking for 'false_positive' inside 10 file(s)...
  No declaration/definition found for function 'false_positive'
