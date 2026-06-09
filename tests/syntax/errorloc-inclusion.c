/* run.config*
   DEPS: errorloc-inclusion-a.h errorloc-inclusion-b.h errorloc-inclusion-c.h
   EXIT: 1
   STDOPT:
*/

#include "errorloc-inclusion-a.h"
#include "errorloc-inclusion-b.h"

void main(void) {
  f();
}
