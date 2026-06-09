/* run.config
  MODULE: @PTEST_NAME@
  OPT: -copy -kernel-warn-key=annot-error=warning
*/

/*@ \test::foo x == 0;
    \test::bar \result == 0;
    \test::bla \trace(x<10) || \trace(x>40);
 */
int f(int x);

/*@ behavior test:
  foo y == 1;
  bar y + \result == 0;
  bla \trace(y<42) && \trace(y>12);
*/
int g(int y);


int f(int x) {
  int s = 0;
  /*@ loop \test::lfoo i<=x;
      loop \test::baz \at(i,LoopEntry), 0;
   */
  for (int i = 0; i < x; i++) s+=g(i);
  /*@ \test::ca_foo s == 0; */
  return s;
}

/*@ behavior ko:
    \test::baz \true;
    baz \true;
*/
int h(int z);

/*@ behavior ko:
    baz \true;
*/
int h2(int z);

int k(int z) {
  int x = z;
  int y = 0;
  /*@ \test::ns_foo \at(x, Post) == z + 1; */
  y = x++;
  return y;
}

/*@ \test::global_foo \forall integer x; x < x + 1
; */

//@ behavior ca_foo: ensures ca_foo: \true;
void loop (void) {
  //@ for ca_foo: \test::ca_foo \true;
  //@ \test::ns_foo \true;
  //@ \test::baz \true;
  //@ \test::empty_extension;
  /*@ loop invariant \true; */
  while (0) { }
}
