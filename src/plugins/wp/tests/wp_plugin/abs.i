/* run.config
 COMMENT: depends on files mentioned in "abs.driver"
 DEPS: @PTEST_DEPS@ abs.why
   OPT: -wp-driver %{dep:@PTEST_DIR@/abs.driver}
 */
/* run.config_qualif
 COMMENT: depends on files mentioned in "abs.driver"
 DEPS: @PTEST_DEPS@ abs.why
   OPT: -wp -wp-driver %{dep:@PTEST_DIR@/abs.driver}
*/
/*@ axiomatic Absolute { logic integer ABS(integer x) ; } */

/*@ ensures \result == ABS(x) ; */
int abs(int x)
{
  if (x < 0) return -x ;
  return x ;
}
