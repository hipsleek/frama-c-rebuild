Test with an already existing mopsa-db.json, to remove dependencies on mopsa.
Note that mopsa-db.json by default always contains absolute paths. We manually
relativized those in our mopsa-db.json to allow them to be versioned.

  $ frama-c -no-autoload-plugins -mopsa-db mopsa-db.json
  [kernel] targets:
    [executable] cJSON_test
    [library   ] libcjson.a
    [library   ] libcjson.so
    [library   ] libcjson.so.1
    [library   ] libcjson.so.1.7.14
    [library   ] libcjson_utils.a
    [library   ] libcjson_utils.so
    [library   ] libcjson_utils.so.1
    [library   ] libcjson_utils.so.1.7.14

Test invalid command lines
  $ frama-c -no-autoload-plugins -mopsa-db test.c # error: invalid JSON file
  [kernel] User Error: mopsa-db: invalid JSON file 'test.c': Line 23, bytes 0-34:
    Invalid token '#include <stdio.h>
    #include <stdl'
  [kernel] Frama-C aborted: invalid user input.
  [1]

  $ frama-c -no-autoload-plugins -mopsa-list-deps cJSON_test
  [kernel:mopsa-db] Warning: 
    library '$TESTCASE_ROOT/libm.a' not found in mopsa-db, ignoring
  [kernel] dependencies:
    $TESTCASE_ROOT/cJSON.c:	 -I '.'
    $TESTCASE_ROOT/test.c:	 -I '.'
  $ frama-c -no-autoload-plugins -mopsa-target cJSON_test
  [kernel:mopsa-db] Warning: 
    library '$TESTCASE_ROOT/libm.a' not found in mopsa-db, ignoring
  [kernel] Parsing cJSON.c (with preprocessing)
  [kernel] Parsing test.c (with preprocessing)
  [kernel:parser:decimal-float] test.c:144: Warning: 
    Floating-point constant 37.7668 is not represented exactly. Will use 0x1.2e226809d4952p5.
    (warn-once: no further messages from category 'parser:decimal-float' will be emitted)
  [kernel:typing:variadic] cJSON.c:1005: Warning: 
    Incorrect type for argument 3. The argument will be cast from int to unsigned int.
