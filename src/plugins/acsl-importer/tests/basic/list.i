/* run.config
STDOPT: -kernel-warn-key annot-error=active -acsl-import %{dep:./list.acsl} -then -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */

/*@
axiomatic A {

  predicate A_is_empty (\list<int> li_V) = \length(li_V) == 0;

  axiom A_length_cons :
    \forall \list<int> li_V, int li, \list<int> li_V1;
      li_V1 == \Cons(li,li_V)
    ==>
      \length(li_V1) == \length(li_V)+1;
}
*/

//@ requires a: \forall int li; \length(\Cons(li,\Nil))==1;
int main (void);
