This test is based on https://github.com/JamesBoer/Tbl. The program is C++,
so Frama-C cannot parse it at all, but this test ensures that at least we
don't crash.

The mopsa-db.json file generated from building the test has been relativized.
Since we don't have any non-C++ parsable sources anyway, we don't even add them
to the test directory.

  $ frama-c -no-autoload-plugins -mopsa-db build/mopsa-db.json -mopsa-target Tests/UnitTests/UnitTests
  [kernel:mopsa-db:non-c-source] Warning: 
    ignoring non-C (C++) dependency: build/Tests/UnitTests/CMakeFiles/UnitTests.dir/UnitTest.cpp.o
    (setting this warning category to inactive or feedback will try to parse it nevertheless)
  [kernel:mopsa-db:non-c-source] Warning: 
    ignoring non-C (C++) dependency: build/Tests/UnitTests/CMakeFiles/UnitTests.dir/TestTable.cpp.o
    (setting this warning category to inactive or feedback will try to parse it nevertheless)
  [kernel:mopsa-db:non-c-source] Warning: 
    ignoring non-C (C++) dependency: build/Tests/UnitTests/CMakeFiles/UnitTests.dir/Main.cpp.o
    (setting this warning category to inactive or feedback will try to parse it nevertheless)
  [kernel:mopsa-db] Warning: 
    No remaining sources in mopsa-db (0 sources before filters)!
