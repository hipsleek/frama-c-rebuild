/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@-2.ya}
 */

void a(void) {}

void main(void)
{
	//@ loop assigns i;
	for (int i=0; i<10; ++i)
		a();
}
