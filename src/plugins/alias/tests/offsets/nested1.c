// nested structures and arrays
//  {z1->s, z2->s, tab_y[0]} are aliased
//  {t->t, z1} are aliased
//  {z1->c, t->d, a} are aliased
//  {z2->c, b} are aliased


#include <stdlib.h>

typedef struct
{       
    int   a;
    int   b;
} st_1_t;

typedef struct
{       
    struct struct_1_t*  s;
    int*   c;
} st_2_t;


typedef struct
{       
    struct struct_2_t*  t;
    int*   d;
} st_3_t;



int main () {

  st_1_t x1 = {0,1};
  st_1_t x2 = {1,2};
  st_1_t *tab_y [2];

  tab_y[0] = &x1;
  tab_y[1] = &x2;
  st_2_t *z1 = malloc(sizeof(st_2_t));
  st_2_t *z2 = malloc(sizeof(st_2_t));
  st_3_t *t = malloc(sizeof(st_3_t));
  int* a = malloc(sizeof(int));
  int* b = malloc(sizeof(int));
  
  *a = 0;
  *b = 5;
  z1->s = tab_y[0];
  z2->s = tab_y[1];
  z1->c = a;
  z2->c = b;
  t->t = z1;
  t->d = a;
  

  return 0;
}
