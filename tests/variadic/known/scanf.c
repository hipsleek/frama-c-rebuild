/* run.config
   STDOPT:
   STDOPT: #"-cpp-extra-args=-DINCLUDE_STDINT"
*/

#include <stdio.h>

int main(){
  char c[10];
  int i;

  scanf("Hello %*10le %% %10s %[^]world] %d !", c, c, &i);

  wchar_t wc;
  scanf("%lc", &wc);

  scanf("%1c", c);
}

#ifdef INCLUDE_STDINT
#include <stdint.h> // avoids warning due to missing intmax_t below
#endif
void warn_about_intmax_t(void) {
  int i;
  scanf("%jd", &i); // '%j' is for intmax_t
}
