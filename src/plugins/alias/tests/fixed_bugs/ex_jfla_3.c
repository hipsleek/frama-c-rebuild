// https://git.frama-c.com/frama-c/frama-c/-/issues/1309

#include <stdio.h>

int* jfla (int *fst, int *snd, int **i1, int **i2, int bo) {
  *snd = *fst;
  if (bo) {
    fst = *i1;
    return *i2;
  }
  else {
    fst = *i2;
    return *i1;
  }
}

int main(void)
{
  int u = 11 ;
  int v = 12 ;
  int t[3] = {0,1,2} ;
  int *a = &t[1];
  int *b = &u ;
  int *c = &v ;
  int **x = &a ;
  int **y = &b ;
  int **z = &c ;
  struct str_t {int *fst; int *snd; } ;
  struct str_t  s = { c , t } ;
  struct str_t *s1 = &s;
  struct str_t *s2 = &s;
  printf("a=%d\n",*s1->fst) ;
  printf("b=%d\n",*s1->snd) ;
  c =  jfla(s1->fst, s1->snd, x, y, 0) ;
  printf("a=%d\n",*s1->fst) ;
  printf("b=%d\n",*s1->snd) ;
  a = jfla(s2->fst, s2->snd, y, z, 1) ;
  printf("a=%d\n",*s1->fst) ;
  printf("b=%d\n",*s1->snd) ;
}

