/*run.config
  DONTRUN: main file is mergecil-weak-3.i
*/

__attribute__((weak)) int f(void) {
  int local_var = 1;
  static int c = 2;
  return local_var - c;
}
