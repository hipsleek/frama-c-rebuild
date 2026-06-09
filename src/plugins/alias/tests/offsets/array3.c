// matrices
//  {x, y} are aliased
 
#include <stdlib.h>


int main () {
  int mat[4][4];
  int* x = malloc (4* sizeof(int));
  int* y = malloc (4* sizeof(int));
  x = mat[0];
  y = mat[1];

  return 0;
}
