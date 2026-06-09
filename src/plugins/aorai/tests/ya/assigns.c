/* run.config*
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya}
   STDOPT: -aorai-automata %{dep:@PTEST_DIR@/assigns_det.ya}
 MODULE: name_projects
 LIBS:
   OPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya} -then -print
*/
int X;

void f(void) { X++; }

/*@ assigns X;
  behavior foo:
  assigns X;
*/
int main () {
  //@ assigns X;
  X++;
  //@ assigns X;
  f();
  return X;
}
