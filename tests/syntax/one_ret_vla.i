int normal(unsigned size) {
  int unused[size];
  return 3;
}

int branch(unsigned size) {
  if (size > 100 || size < 4) return 3;
  int unused[size];
  unused[size-1] = 42;
  return 2;
}

int f(unsigned size) {
  return 3;
  int unused[size];
  size++;
}

int g(unsigned size) {
  return 3;
  int unused[size];
}

int h(unsigned size) {
  return 3;
  {
    int unused[size];
    return 4;
  }
}
