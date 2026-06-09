/* run.config
PLUGIN: rtegen
   OPT: -print -machdep %{dep:@PTEST_DIR@/machdep_char_unsigned.yaml} -then -constfold -rte
*/
char t[10];

void main() {
  int r = (t[0] == 'a');
  char c = 455;
}
