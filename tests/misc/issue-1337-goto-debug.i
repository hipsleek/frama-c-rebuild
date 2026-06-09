/* run.config
  OPT: -kernel-msg-key typing
*/

int x;

void f() {
  goto L;
  x++;
  L: x++;
  return;
}
