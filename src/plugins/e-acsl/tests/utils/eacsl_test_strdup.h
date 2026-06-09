#include <stdlib.h>

/**
 * Replacement for `strdup()` with an explicit size argument. The size argument
 * must include the `\0` character of the original string.
 *
 * The function's goal is to replace `strdup()` calls in test setups. The
 * original `strdup()` function cannot be verified by Eva when combined with
 * E-ACSL's code generation: the post-condition for the allocation behavior is
 * found false. This implementation uses an explicit size argument and provides
 * the C code to Eva so that it can be verified without relying on axiomatic
 * specifications.
 */
char * eacsl_test_strdup(const char * src, size_t size) {
  char * res = malloc(size);
  //@ assert res != NULL;
  for (int i = 0; i < size; ++i) {
    res[i] = src[i];
  }
  return res;
}
