/* run.config_qualif
   DONTRUN:
*/

int *p;

/*@
  axiomatic Observer {
    predicate H{S}(int x) reads *p;
    predicate P{S}(int x) reads *p;
    predicate Q{S}(int x) reads *p;
    predicate W{S}(int x) reads *p;
  }
*/

/*@
  assigns *s,*p;
  behavior A:
  assumes  H(*s);
  requires P(*s);
  ensures  Q(*s);
*/
int f(int *s);

int g(int *s) {
  int r = f(s);
  /*@ assert W(*s); */
  return r;
}
