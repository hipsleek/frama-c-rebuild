/* run.config
   MODULE: @PTEST_NAME@
   OPT: -create-proj other -print-proj -then-on other -print-proj
   OPT: -create-proj other -print-proj -then-on="other" -print-proj
   EXIT: 1
   OPT: -create-proj other -print-proj -then-on
*/

int main() { return 0; }
