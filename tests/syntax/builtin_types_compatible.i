/* run.config
   OPT: -machdep gcc_x86_64 -print
*/
int main() {
  return __builtin_types_compatible_p(int, int[]);
}
