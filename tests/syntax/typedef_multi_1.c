/* run.config
 MODULE: typedef_multi
 DEPS: typedef_multi.h
   OPT: -no-autoload-plugins %{dep:./typedef_multi_2.c}
*/
#include "typedef_multi.h"

void f () {  while(x<y) x++; }
