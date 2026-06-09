/* run.config
 DEPS: static_formals.h
   STDOPT: +"%{dep:./static_formals_2.c}" +"-cpp-extra-args=\"-I ./\"" +"-kernel-msg-key printer:vid"
*/
#include "static_formals.h"
int g() { return f(4); }
