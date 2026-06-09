#include <wctype.h>

int main() {
  wctype_t wt = wctype("digit");
  int b = iswctype(L'0', wt);
  return WEOF;
}
