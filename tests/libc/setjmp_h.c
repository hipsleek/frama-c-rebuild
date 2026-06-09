/* run.config
 */

/* NOTE: this header is NOT semantically supported by Frama-C; but to ensure
   we can at least parse code using it, this test should help avoid
   regressions.
*/

#include <setjmp.h>

volatile int nondet;
extern jmp_buf jbuf;

void jump() {
  longjmp(jbuf, 1);
  //@ assert unreachable: \false;
}

int come_from() {
  int res1 = setjmp(jbuf);
  // The code below only works if jmp_buf and sigjmp_buf are the same type;
  // POSIX does not require it, but glibc/musl do it, so we must ensure
  // such code will parse.
  int res2 = sigsetjmp(jbuf, 1);
  return res1 + res2;
}

int main() {
  if (come_from() > 1) {
    return 0;
  }
  jump();
}
