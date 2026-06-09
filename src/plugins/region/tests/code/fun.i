int foo(int);
int bar(int);

int job(int x) {
  int (*fn)(int) = 0 <= x ? foo: bar ;
  //@ calls foo,bar ;
  return fn(x);
}
