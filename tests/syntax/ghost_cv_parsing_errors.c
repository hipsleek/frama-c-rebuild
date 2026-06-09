/* run.config
 EXIT: 1
   OPT:-cpp-extra-args="-DIN_TYPE"
   OPT:-cpp-extra-args="-DIN_DECL"
   OPT:-cpp-extra-args="-DIN_GHOST_ATTR"
*/

// All of this should be refused

#ifdef IN_TYPE

struct S {
  int a ;
} \ghost ;

#endif

#ifdef IN_DECL

int \ghost global ;

#endif

#ifdef IN_GHOST_ATTR

// Attribute annotation are not supported anymore, any keyword or identifier
// Here will produce the same error.
int /*@ \ghost */ global ;

#endif
