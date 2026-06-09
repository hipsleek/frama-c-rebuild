/*run.config
  OPT: -print %{dep:./mergecil-weak-2.i} %{dep:./mergecil-weak-1.i}
  OPT: -print %{dep:./mergecil-weak-1.i}
  OPT: -print %{dep:./mergecil-weak-2.i}
EXIT: 1
  OPT: %{dep:./mergecil-weak-1.i} %{dep:./mergecil-weak-2.i}
*/

int f(void);

int main() {
  int local_var = 42;
  int r = f() + f();
  return r + local_var;
}
