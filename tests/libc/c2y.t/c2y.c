#include <string.h>

// ALIGN defined via -cpp-extra-args
const int align = ALIGN;

int main() {
  bool b1 = _Alignof(int) > 1 && true || false;
  alignas(ALIGN) char aligned;

  char a;
  memcpy(&a, 0, 0); // allowed in C2y
  memcpy(0, 0, 0); // allowed in C2y
  strncpy(0, &a, 0); // allowed in C2y
}
