void f() {
  struct { int x; int y; int* z; short arr[4]; } s;
  int a = 1;
  s.x = a;
  s.z = &a;
  s.arr[1] = 0;
}
