// function with no return
//  {a, b} are aliased
//  {c, d} are aliased
//#include <stdio.h>
#include <stdlib.h>

void alias(int **x, int **y) {
  *x = *y;
}


int main(void)
{
  int *a = malloc(sizeof(int));
  *a = 0;
  int *b = malloc(sizeof(int));;
  *b = 42;
  
  alias(&a,&b);

  *a = 7;
  //printf("a = %d && b = %d\n",*a,*b);


  return 0;
}
