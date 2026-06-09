#include <wchar.h>

typedef int test[sizeof("string-lit-typedef")];

struct S {
  int test[sizeof("string-lit-field")];
};

test a = { 1 };

struct S s;

int main() {
  char test = "string literal"[3];
  /*@ assert test == 'i'; */
  wchar_t ltest = L"wide string literal"[5];
  /*@ assert ltest == L's'; */
  const char (*atest)[] = &"address of string literal";
  const wchar_t (*latest)[] = &L"address of wide string literal";
  /*@ assert (*atest)[0] == (*latest)[0]; */
  s.test[0] = 42;
}
