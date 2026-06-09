/* run.config
   STDOPT:
*/

#include <stdlib.h>

__attribute__((__noreturn__)) void f1() {
  exit(0);
}

__attribute__((__noreturn__)) void f2(){
  __attribute__((__noreturn__)) void (*funptr)(void) = &f1;
  (*funptr)();
}

_Noreturn void g1() {
  exit(0);
}

_Noreturn void g2(){
  _Noreturn void (*funptr)(void) = &g1;
  (*funptr)();
}

