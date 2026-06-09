// https://git.frama-c.com/frama-c/frama-c/-/issues/1299
// crashes due to an incoherent graph

void f() {
  int *q = (int*)f, *p = (int*)f;
  p = (int*)&q;
}
