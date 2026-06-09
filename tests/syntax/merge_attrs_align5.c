/* run.config
   DONTRUN: main test is in merge_attrs_align.c
*/

typedef short __attribute__((__aligned__(1))) packed_short;

typedef struct {
  char a;
  packed_short b; // offset: 1
} s;

extern s s1;

// for testing with GCC/Clang
#ifndef __FRAMAC__
#include <stddef.h>
#include <stdio.h>
#endif
int f5() {
  char c = s1.a;
#ifndef __FRAMAC__
  printf("f5: offsetof b = %lu\n", offsetof(s, b));
#endif
  return 0;
}
