/* run.config
 DEPS: dec.h
   STDOPT: +"%{dep:./use2.c}"
*/

// BTS 0887

#include "dec.h"

//@ ensures X > 0 ; ensures F(1) > 0 ;
void f(void) {}
