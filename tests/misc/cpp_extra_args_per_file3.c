/* run.config
   DONTRUN: main test is cpp-extra-args-per-file1.c
 */

const char *const version = VERSION;

int f(void) {
  return version[10];
}
