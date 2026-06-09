/* run.config
 DEPS: merge_logic_globals.h
  OPT: -print %{dep:./merge_logic_globals_2.c} -cpp-extra-args="-I./"
*/

#include "merge_logic_globals.h"

int main() { test(); /*@ assert p((int)li); */ }
