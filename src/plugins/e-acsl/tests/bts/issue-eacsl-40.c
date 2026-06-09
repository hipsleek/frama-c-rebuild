/* run.config
   COMMENT: frama-c/e-acsl#40, test for initialized memory after a call to fread
   on /dev/urandom.
   STDOPT:
*/
/* run.config_dev

   MACRO: ROOT_EACSL_GCC_OPTS_EXT -c

*/

/* In some GCC/libc versions, GCC uses 'malloc' function attributes with
   deallocator names; if these deallocators have been cleaned up by Rmtmps, the
   resulting code will not parse. To avoid that, such attributes are not
   reprinted by Frama-C's kernel. The test below checks whether code produced by
   E-ACSL's instrumentation remains parsable by GCC.
*/

#include <stdio.h>

int main() {
  char buf[4];
  FILE *f = fopen("/dev/urandom", "r");
  if (f) {
    char buf[4];
    int res = fread(buf, 1, 4, f);
#ifndef NO_FCLOSE
    fclose(f);
#endif
    if (res == 4) {
      //@ assert \initialized(&buf[3]);
      buf[0] = buf[3];
    } else
      return 2;
  } else
    return 1;
  return 0;
}
