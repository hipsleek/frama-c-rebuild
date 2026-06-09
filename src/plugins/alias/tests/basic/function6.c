// function with no return
// no alias


void swap(int *x, int* y) {
  int z=0;
  z=*x;
  *x=*y;
  *y=z;
}


int main(void)
{
  int *a=0, *b=0, *c=0, *d=0;
  swap(a,b);
  swap(c,d);
  return 0;
}
