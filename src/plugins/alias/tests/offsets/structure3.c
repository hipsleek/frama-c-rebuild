// double structure with initialisation and pointer

// There are no real aliases at the end of the function,
// but some aliases are created along the way,
// so at the end we obtain { (z.t)->s; y1.s; y2.s }

typedef struct st_1_t {
    int   a;
    int   b;
} st_1_t;

typedef struct st_2_t {
    struct st_1_t*  s;
    int   c;
} st_2_t;

typedef struct st_3_t {
    struct st_2_t*  t;
    int   d;
} st_3_t;

int main () {
  st_1_t x1 = {0,1};
  st_1_t x2 = {1,2};
  st_2_t y1 = {&x1,3};
  st_2_t y2 = {&x2,4};
  st_3_t z = {&y1,5}; // creates alias { (z.t)->s; y1.s }

  z.t = &y2; // creates alias { (z.t)->s; y2.s }
  y1.c = z.d;

  return 0;
}
