#ifdef __FRAMAC__
int main() {
  return 0;
}
#else
#include <iostream>

int main() {
  std::cout << "hello" << std::endl;
  return 0;
}
#endif
