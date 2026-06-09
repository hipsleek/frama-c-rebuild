/* run.config
   STDOPT: +"%{dep:./aggressive_merging_2.i} -aggressive-merging"
*/
static inline void f(void) {
  return;
 }

void foo (void)
{
  f();
}
