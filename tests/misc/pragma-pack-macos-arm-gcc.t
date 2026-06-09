Testing that the output produced by Eva and the execution of the binary
compiled by GCC are identical, on a macos-ARM machdep

  $ frama-c pragma-pack.c -machdep macos_arm -eva -eva-msg-key=-summary | grep -A999 "eva:final-states" | grep -v "\[eva:final-states\]" | grep -v __retres > eva.res
  $ gcc pragma-pack.c -Wno-pragmas && ./a.out > gcc.res
  $ diff -B eva.res gcc.res # should be identical
