// homogeneous cast
//  {a, c} are aliased
//  {b, d} are aliased


int main () {

  int *a=0, *b=0;
  float *c=0, *d=0;
  a = (int*) c;
  d = (float*) b;
  
  return 0;
}
