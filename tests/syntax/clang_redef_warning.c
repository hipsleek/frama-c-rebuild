/* run.config
ENABLED_IF: %{bin-available:clang}
OPT: -cpp-command="clang -C -E -I." -cpp-frama-c-compliant -print
*/

#include <stddef.h>

void f(void) { }
