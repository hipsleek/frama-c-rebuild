// https://git.frama-c.com/frama-c/frama-c/-/issues/1309

#include <stdio.h>
#include <stdlib.h>


typedef struct {int *a; int *b;} str_t;

str_t* create_str_t (int va, int *b) {
  str_t *res = malloc(sizeof(str_t));
  if (res) {
    res->a = malloc(sizeof(int));
    res->b = malloc(sizeof(int));
    if (res->a)
      *res->a = va;
    if (res->b)
      res->b = b;
  }
  return res;
}

void jfla (str_t *s, int* i1, int* i2, int b) {
  *s->b = *s->a;
  if (b)
    s->a = i1;
  else
    s->a = i2;
}


int main(void)
{
  int u = 11 ;
  int v = 12 ;
  int t[3] = {0,1,2} ;
  int *a = t+1;
  int *b = &u ;
  int *c = &v ;
  int **w= &a ;
  str_t *s1 = create_str_t(-1,b);
  if (s1) {
    printf("a=%d\n",*s1->a) ;
    printf("b=%d\n",*s1->b) ;
    jfla(s1, a, t+2, 1) ;
    printf("a=%d\n",*s1->a) ;
    printf("b=%d\n",*s1->b) ;
    jfla(s1, t, c, 0) ;
    printf("a=%d\n",*s1->a) ;
    printf("b=%d\n",*s1->b) ;
  }
}
