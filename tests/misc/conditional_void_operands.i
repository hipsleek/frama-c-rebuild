/* run.config
   OPT: -no-autoload-plugins -print
*/

void f(int x) {
  // ISO 6.5.15#3 : both operands have void type
  void a, b;
  x ? a : b;
}
