/* run.config
   EXIT: 0
   STDOPT: -machdep x86_64
   STDOPT: -machdep x86_32
*/

#include <assert.h>

char c = 1;
_Static_assert(sizeof(-c) == sizeof(int), "Integer promotion with minus");
_Static_assert(sizeof(+c) == sizeof(int), "Integer promotion with plus");
_Static_assert(sizeof (c) == sizeof(char), "sizeof numeric char");
char a = 'a';
_Static_assert(sizeof(a) == sizeof(char), "sizeof char");
_Static_assert(sizeof ('a') == sizeof(int), "sizeof literal char");

#include "stdint.h"

int8_t i8 = INT8_C(125);
uint8_t u8 = UINT8_C(230);
_Static_assert(sizeof +i8 == sizeof +INT8_C(50), "i8");
_Static_assert(sizeof +u8 == sizeof +UINT8_C(140), "u8");

int16_t i16 = INT16_C(864);
uint16_t u16 = UINT16_C(45213);
_Static_assert(sizeof +i16 == sizeof +INT16_C(645231), "i16");
_Static_assert(sizeof +u16 == sizeof +UINT16_C(8475), "u16");

int32_t i32 = INT32_C(864531);
uint32_t u32 = UINT32_C(45213);
_Static_assert(sizeof +i32 == sizeof +INT32_C(645231), "i32");
_Static_assert(sizeof +u32 == sizeof +UINT32_C(8475), "u32");

int64_t i64 = INT64_C(753);
uint64_t u64 = UINT64_C(6453);
_Static_assert(sizeof +i64 == sizeof +INT64_C(6531), "i64");
_Static_assert(sizeof +u64 == sizeof +UINT64_C(64751), "u64");

int v = 645201;
_Static_assert(sizeof +v == sizeof(int), "Integer is promote to integer");

float f = 1.;
_Static_assert(sizeof +f == sizeof(float), "Float promotion");
double d = 1.;
_Static_assert(sizeof +d == sizeof(double), "Double promotion");

void vla(int n) {
  unsigned long s = sizeof(int[n]);
  assert(s == n * sizeof(int));
}

// We drop side-effects but keep the temporary variable
int effects(int x){
  int res;
  switch (x) {
    case sizeof((int[]){ 1, 2, 3 }): res = 1;
    default: res = sizeof((int[]){ 1, 2, 3 });
  }
  return res;
}
