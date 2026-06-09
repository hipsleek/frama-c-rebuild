#include <stdlib.h>
#include <X>

volatile int v;

int main() {
  asm("mov ax, ax");
  if (v) return main();
  malloc(42);
  return 0;
}
