void alias_xy(int *x, int* y, int* z) {
  x=y;
}

void alias_yz(int *x, int* y, int* z) {
  y=z;
}

int main(void)
{
  int *a=0, *b=0, *c=0;
  void (*p)(int* x, int* y, int* z);
	p = &alias_xy;
  p = &alias_yz;
  (*p)(a,b,c);
  return 0;
}
