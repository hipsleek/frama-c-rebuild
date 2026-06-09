struct S { int f, g; } ;

void job1(struct S a) {
  struct S s[4] = { [1]=a, 42 };
}

//@ region a, \garbage;
void job2(struct S a, int k) {
  struct S s[4] = { [1]=a, 42 };
}

//@ region k, \garbage;
void job3(struct S a, int k) {
  struct S s[4] = { [1]=a, 42 };
}
