/* run.config
STDOPT: -no-unicode -acsl-import %{dep:./bts-1546.acsl} -print -acsl-import-debug 2 -acsl-import-msg-key "*"
 */
enum e0 { E0 =  0 }; // underlying type: unsigned int
enum e1 { E1 = -1 }; // underlying type: signed int

/*@ ensures P1: x==E0 ==> \result==E0;
    ensures P2: \result==x;*/
enum e0 f(enum e0 x) { return x ; }

/*@ behavior B:
      assumes H: s == u;
      ensures P: \result == ((unsigned) E0);
*/
enum e0 g0(enum e0 x, signed s, unsigned u) {
  if (s == u)
    return (unsigned) E0 ;
  if (x == u)
    return 1 ;
  if (x == s)
    return 2 ;
  return 0 ;
}

/*@ behavior B:
      assumes H: s == u;
      ensures P: \result == ((unsigned) E0);
*/
enum e1 g1(enum e1 x, signed s, unsigned u) {
  if (s == u)
    return (unsigned) E1 ;
  if (x == u)
    return 1 ;
  if (x == s)
    return 2 ;
  return 0 ;
}
