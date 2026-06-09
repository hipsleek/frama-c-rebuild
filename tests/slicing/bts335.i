/* run.config
   STDOPT: +"-slice-annot g -calldeps -slicing-level 3 -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -no-calldeps"
*/

/*
bin/toplevel.opt -slice-annot g -calldeps -slicing-level 3 %{dep:./bts335.c} -debug 2
bin/toplevel.opt -pdg-debug -pdg -pdg-debug "-pdg-pot bts335" %{dep:./bts335.c} 
 */
int T[2]  = {0, 0};
void f (int i) { T[i]++; }
void g (void) { f(0); /*@ slice_preserve_expr T[0]; */ }
void main (int c) { if (c) g(); else f(1); }
