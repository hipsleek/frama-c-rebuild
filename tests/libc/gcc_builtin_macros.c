/* run.config
   STDOPT: #"-machdep gcc_x86_64 -cpp-extra-args='-include __fc_gcc_builtin_macros.h'"
*/

volatile int nondet;

int main(void) {
  if (nondet) __builtin_abort();
  int i = __builtin_abs(-1);
  double d = __builtin_acos(1);
  long long ll = __builtin_imaxabs(-2);
  double d2 = __builtin_pow(4.0, 0.5);
  int b = __builtin_isalnum('Z') & __builtin_iswlower(L'a');
  unsigned long s = __builtin_strlen("42");
  char *p = __builtin_malloc(4);
  return 0;
}
