/* run.config
   COMMENT: typedef (from a Bernard's bug report)
   STDOPT: +"-machdep gcc_x86_64"
*/

typedef unsigned char uint8;

int main(void) {
  uint8 x = 0;
  /*@ assert x == 0; */;
  __int128 i = x + 1;
  //__uint128_t u = i - 2;
  unsigned __int128 u2 = i + 2;
  unsigned __int128 u3 = u2 + 18446744073709551615U;
  return 0;
}
