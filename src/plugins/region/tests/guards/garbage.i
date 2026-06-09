struct S { int f; };

int local(int v) {
  struct S y = { v };
  return y.f;
}

int safe(struct S x) {
  struct S y = x;
  return y.f;
}

//@ region x, \garbage;
int risky(struct S x) {
  struct S y = x;
  return y.f;
}
