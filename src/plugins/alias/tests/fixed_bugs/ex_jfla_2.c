// https://git.frama-c.com/frama-c/frama-c/-/issues/1309

#include <stdio.h>
#include <stdlib.h>


typedef struct {int *fst; int *snd;} str_t;

str_t* create_str_t (int va, int *b) {
  str_t *res = malloc(sizeof(str_t));
  if (res) {
    res->fst = malloc(sizeof(int));
    res->snd = malloc(sizeof(int));
    if (res->fst)
      *res->fst = va;
    if (res->snd)
      res->snd = b;
  }
  return res;
}

void jfla (str_t *s, int* i1, int* i2, int b) {
  *s->snd = *s->fst;
  if (b)
    s->fst = i1;
  else
    s->fst = i2;
}

int main(void)
{
  int u = 11 ;
  int v = 12 ;
  int t[3] = {0,1,2} ;
  int *a = t+1;
  int *b = &u ;
  int *c = &v ;
  int **x = &a ;
  int **y = &b ;
  int **z = &c ;
  str_t *s1 = create_str_t(-1,b) ;
  str_t *s2 = create_str_t(-2,c) ;
  printf("a=%d\n",*s1->fst) ;
  printf("b=%d\n",*s1->snd) ;
  jfla(s1, a, t+2, 1) ;
  printf("a=%d\n",*s1->fst) ;
  printf("b=%d\n",*s1->snd) ;
  jfla(s1, t, s2->snd, 0) ;
  printf("a=%d\n",*s1->fst) ;
  printf("b=%d\n",*s1->snd) ;
}
