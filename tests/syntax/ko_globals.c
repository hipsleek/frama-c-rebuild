/* run.config
   EXIT: 1
   STDOPT:
*/

#include <stdint.h>

int * const a;
int * b = a;

uintptr_t b1 = (uintptr_t) a;

struct stru {
    const int c;
};

struct stru d = { 0 };

int e = d.c;

const int f = f;

int g = g;

short h = (short) &h;

const int i;
int j = i;

int k = 1;
int l = k;

struct stru2 {
    int m;
};

int n = 1;
struct stru2 o = { n };

union unio2 {
    int p;
};

int q = 1;
union unio2 r = {q};
