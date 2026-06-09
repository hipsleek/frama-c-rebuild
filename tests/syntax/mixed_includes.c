/* run.config
   ENABLED_IF: %{bin-available:gcc}
   DEPS: ./mixed_includes/limits.h
   OPT: -cpp-command="gcc -E -C -Imixed_includes" -print
*/

#include <stdlib.h>

int main() {
  if (INT_MAX != 1000000000)
    abort();
}
