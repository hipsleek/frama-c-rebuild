// for loop
// no alias

#include <stdlib.h>

int main ()
{
  int* s = 0;
  for (int idx = 0; idx < 10; idx++) {
    s = malloc(idx);
  }
  return 0;
}
