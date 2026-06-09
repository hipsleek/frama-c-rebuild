/* run.config
  STDOPT: +"-kernel-warn-key typing:implicit-int=feedback"
*/

/* It is not allowed by ISO C11 6.7.2.2 to omit type specifier in a decaration,
   but gcc and clang allow it with -Wimplicit-int. */

const c;

volatile v;

extern f() { return c + v; }

_Noreturn g(void) { while(1); }
