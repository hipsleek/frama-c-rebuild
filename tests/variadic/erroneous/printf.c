#include <stdio.h>

int main()
{
  double d = 2;
  char* c = "toto";

  printf("Hello %-- 0+#20.10le %% %s world %d !", d, c, 42);

  int i = 3;
  printf("% +f % +d", d, i); // technically not UB according to C23, but the
                             // space is ignored, and GCC/Clang warn about it
}
