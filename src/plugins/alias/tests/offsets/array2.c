// matrices (this example may be not a valid c program)
//  no alias

int main () {
  int mat[4][4];
  mat[0][0] = 0;
  mat[0][1] = 1;

  int **x;
  *x = mat[1];

  int* y;
  *y = *x[0];

  
  return 0;
}
