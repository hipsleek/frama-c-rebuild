#include <wchar.h>

_Static_assert(sizeof(L"AA") == 3*sizeof(wchar_t), "Incorrect sizeof behaviour");

_Static_assert(sizeof(L"\123\456") == 3*sizeof(wchar_t), "Incorrect handling of escape sequences");

int f(const wchar_t* s) {
  return wcscmp(s,L"");
}

int main(void){}
