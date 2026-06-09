// structure with malloc
//  {z->s, y1} are aliased

#include <stdlib.h>

typedef struct
{       
    int   a;
    int   b;
} st_1_t;

typedef struct
{       
    struct struct_1_t*  s;
    int   c;
} st_2_t;


typedef struct
{       
    struct struct_2_t*  t;
    int   d;
} st_3_t;





int main () {

  st_1_t x1 = {0,1};
  st_1_t x2 = {1,2};
  st_1_t *y1 = malloc(sizeof(st_1_t));
  st_2_t *z = malloc(sizeof(st_2_t));

  y1 = &x1;
  z->s = y1;
  z->c = 6;

  return 0;
}
