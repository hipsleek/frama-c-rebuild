Testing that the output produced by Eva and the execution of the binary
compiled by GCC are identical, both on a 32-bit machdep and on a 64-bit one

# Note: we cannot currently test this in Nix due to issues with multiStdenv
#  $ frama-c pragma-pack.c -machdep gcc_x86_32 -eva -eva-msg-key=-summary | grep -A999 "eva:final-states" | grep -v "\[eva:final-states\]" | grep -v __retres > eva.res
#  $ gcc -m32 pragma-pack.c -Wno-pragmas && ./a.out > gcc.res
#  $ diff -B eva.res gcc.res # should be identical

  $ frama-c pragma-pack.c -machdep gcc_x86_64 -eva -eva-msg-key=-summary | grep -A999 "eva:final-states" | grep -v "\[eva:final-states\]" | grep -v __retres > eva.res
  $ gcc pragma-pack.c -Wno-pragmas && ./a.out > gcc.res
  $ diff -B eva.res gcc.res # should be identical
