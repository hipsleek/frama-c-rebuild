/* run.config_dev
   MACRO: ROOT_EACSL_EXEC_EXIT_CODE 134
*/

#include <sys/types.h>

int main() {
  char chars[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
  size_t c = _Alignof(char);
  //@ assert \aligned(&chars[0], c);
  //@ assert \aligned(&chars[1], c);
  //@ assert \aligned(&chars[0], 2 * c) || \aligned(&chars[1], 2 * c);
  //@ assert !\aligned(&chars[0], 2 * c) || !\aligned(&chars[1], 2 * c);

  int *i0 = (int *)(chars + 0);
  int *i1 = (int *)(chars + 1);
  //@ assert !\aligned(i0, alignof(int)) || !\aligned(i1, alignof(int));

  // fails RTE check: alignment should not be zero
  //@ assert \aligned(i0, 2 - 2);

  return 0;
}
