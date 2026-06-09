/* run.config*
 EXIT: 1
   STDOPT: #"-cpp-extra-args=-DERR_VA_ARG"
   STDOPT: #"-cpp-extra-args=-DERR_VA_COPY"
   STDOPT: #"-cpp-extra-args=-DERR_VA_START"
 COMMENT: code inspired by GCC's testsuite
*/

#include <stdarg.h>
int f(int n, ...) {
  char **a = 0;
#ifdef ERR_VA_ARG
  va_arg(a, char **);
#endif
#ifdef ERR_VA_COPY
  va_copy(a, a);
#endif
#ifdef ERR_VA_START
  va_start(a, n);
#endif
  // No need to test va_end, nothing is done in its translation
  return 0;
}
