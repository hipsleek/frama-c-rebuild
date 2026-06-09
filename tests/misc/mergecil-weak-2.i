/*run.config
  DONTRUN: main file is mergecil-weak-3.i
*/

int f(void) {
  int local_var = 1;
  static int b = 42;
  return b+local_var;
}
