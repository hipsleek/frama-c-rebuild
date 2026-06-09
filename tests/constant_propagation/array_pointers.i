/* run.config
   COMMENT: We need GCC machdep for this test because of arithmetic on void*
   OPT: @EVA_OPTIONS@ -eva -machdep gcc_x86_32  -then -scf
*/

void *p;

void main() {
  void **q = &p+1;
  void **r = q+1;
  void *s = p + 1;
}
