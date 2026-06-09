/* run.config*
OPT: -slice-annot main -then-last -print
*/
int g(int x) { return x; }

int main() {
  /*@ assert &g == &g; */
  /*@ slice_preserve_stmt; */
  g(0);
}
