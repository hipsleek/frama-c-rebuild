/* run.config*
   STDOPT: +"-machdep gcc_x86_64 -cpp-extra-args=\"-include __fc_machdep.h\""
*/

__int128_t shiftr(__uint128_t x) {
  return x >> 2;
}

int main() {
  unsigned __int128 u = -1;
  __int128 i = u / 2;
  __uint128_t m = (i - u) % 0xffffffffffffffff;
  return shiftr(m);
}
