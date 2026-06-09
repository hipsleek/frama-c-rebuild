/* run.config
   PLUGIN:
   OPT: -constfold -print
*/

#include <wchar.h>

const char test[] = "literal string";

const wchar_t wtest[] = L"wide string literal";

int f() {
  return test[0] + test[1];
}

int wf() {
  return wtest[2] + wtest[3];
}

int upper_limit() { return test[14]; }
