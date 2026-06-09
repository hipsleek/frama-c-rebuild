/* run.config
 EXIT: 1
 DEPS: multiple_include.h
   OPT: -kernel-warn-key=annot-error=warning -print %{dep:./multiple_include_1.c}
*/

#include "multiple_include.h"

/*@ requires p(x); */
void bar(int x) { i+=x; return; }
