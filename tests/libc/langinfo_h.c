#include <langinfo.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
  setlocale(LC_CTYPE, "");
  setlocale(LC_NUMERIC, "");

  printf("%s\n", nl_langinfo(CODESET));
  printf("%s\n", nl_langinfo(RADIXCHAR));

  return EXIT_SUCCESS;
}
