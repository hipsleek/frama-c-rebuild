/* run.config
   STDOPT: +""
   STDOPT: +"-keep-unused-functions all"
 */

/* Tests AST consistency on inline functions,
   including when Rmtmps is disabled. */

/*@ assigns \result \from x;
    ensures \result == x; */
inline int id(int x) { return x; }

inline int incr (int x) { return x + 1; }

// Removed unless "-keep-unused-functions all" is set.
inline int decr (int x) { return x - 1; }

void main (void) {
  int a = incr(41);
}
