#include <uchar.h>

int main() {
  mbstate_t st = {0};
  size_t r;
  char buf[10];

  char8_t pc8[10];
  char8_t c8 = {0};
  r = mbrtoc8(pc8, "bla", 3, 0);
  r = mbrtoc8(pc8, "bla", 3, &st);
  r = mbrtoc8(0, 0, 3, &st);
  r = mbrtoc8(0, "", 1, 0);
  r = c8rtomb(buf, c8, &st);
  r = c8rtomb(0, c8, &st);
  r = c8rtomb(0, c8, 0);

  char16_t pc16[10];
  char16_t c16 = {0};
  r = mbrtoc16(pc16, "bla", 3, 0);
  r = mbrtoc16(pc16, "bla", 3, &st);
  r = mbrtoc16(0, 0, 3, &st);
  r = mbrtoc16(0, "", 1, 0);
  r = c16rtomb(buf, c16, &st);
  r = c16rtomb(0, c16, &st);
  r = c16rtomb(0, c16, 0);

  char32_t pc32[10];
  char32_t c32 = {0};
  r = mbrtoc32(pc32, "bla", 3, 0);
  r = mbrtoc32(pc32, "bla", 3, &st);
  r = mbrtoc32(0, 0, 3, &st);
  r = mbrtoc32(0, "", 1, 0);
  r = c32rtomb(buf, c32, &st);
  r = c32rtomb(0, c32, &st);
  r = c32rtomb(0, c32, 0);

  return 0;
}
