// nested structures and arrays
//  {t->t, z1} are aliased
//  {z1->c, t->d, a} are aliased


#include <stdlib.h>

typedef struct
{       
    int   a;
    int   b;
} st_1_t;

typedef struct
{       
    struct struct_1_t*  s [2];
    int*   c;
} st_2_t;


typedef struct
{       
    struct struct_2_t*  t;
    int*   d;
} st_3_t;





int main () {

  st_1_t x1 = {0,1};
  st_1_t x2 = {2,3};

  st_2_t *z1 = malloc(sizeof(st_2_t));

  st_3_t *t = malloc(sizeof(st_3_t));
  int* a = malloc(sizeof(int));
  
  *a = 0;
  z1->s[0] = &x1;
  z1->s[1] = &x2;
  z1->c = a;
  t->t = z1;
  t->d = a;
  

  return 0;
}
