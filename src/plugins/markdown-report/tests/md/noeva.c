/*run.config
PLUGIN: markdown-report
CMD: @frama-c@
OPT: -mdr-gen md -mdr-date="now" -mdr-out @PTEST_NAME@.@PTEST_NUMBER@.md
*/

//since we do not launch Eva, no alarm should be reported.

#include <stddef.h>
int f(int* x) { return *x; }
int main() { return f(NULL); }
