/* run.config
   OPT: -machdep gcc_x86_64 -print
*/
int f() {
  int x = __builtin_choose_expr(sizeof(char) == 1, 42, 3.f);
  float y = __builtin_choose_expr(sizeof(char) == 1, 3.f, 42);
  return x;
}
