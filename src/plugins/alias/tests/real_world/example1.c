// real world example
//  {a->t1[0], b->t1[0]} are aliased
//  {a->t2[0], b->t2[0]} are aliased
//  {a->n1, b->n1} are aliased
//  {a->n2, b->n2} are aliased


#include <stdlib.h>
#include <math.h>

typedef struct {
  double *t1[2];
  double *t2[2];
  int *n1;
  int *n2;
} ty;

void swap(int *n)
{
  if ((*n) == 1) {
    (*n) = 0;
  }
  else {
    (*n)++;
  }
}


void *f1(ty *x)
{
  ty *tmp = x;
  double *idata;
  double *odata;
  int idx;

  idata = (double *)malloc(10 * sizeof(double));

  while (1) {

    idata = tmp->t2[*tmp->n2];

    odata = tmp->t1[*tmp->n1];

    for (idx = 0; idx < 10; idx++) {
      odata[idx] = 0.5*sin(idata[idx]);
    }
    swap(tmp->n1);
  }
  return (void *) NULL;
}


void *f2(ty *x)
{
  ty *tmp = x;
  int idx;
  double *idata;
  double *odata;

  idata = (double*)malloc(10* sizeof(double));

  while (1) {

    idata = tmp->t1[*tmp->n1];

    odata = tmp->t2[*tmp->n2];

    for (idx = 0; idx < 10; idx++) {
          odata[idx] = 3*idata[idx] + 1;
    }
    swap(tmp->n2);
  }

  return (void *) NULL;
}


int main(void)
{
  ty *a;
  ty *b;

  a = (ty *)malloc(sizeof(ty));
  b = (ty *)malloc(sizeof(ty));

  int i;
  for (i = 0; i < 2; i++) {
      a->t1[i] = (double *)malloc(10 * sizeof(double));
      a->t2[i] = (double *)malloc(10 * sizeof(double));
  }

  a->n1 = (int *)malloc(sizeof(int));
  a->n2 = (int *)malloc(sizeof(int));
  *a->n1 = 1;
  *a->n2 = 1;

  for (i = 0; i < 2; i++) {
    b->t1[i] = a->t1[i];
    b->t2[i] = a->t2[i];
  }
  b->n1 = a->n1;
  b->n2 = a->n2;

  f1(a);
  f2(b);

  return 0;
}
