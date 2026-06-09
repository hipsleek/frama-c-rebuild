int* jfla (int *fst, int *snd, int **i1, int **i2, int bo) {
  *snd = *fst;
  if (bo) { fst = *i1; return *i2; }
  else    { fst = *i2; return *i1; }
}

void main(void) {
  int u = 11, v = 12, t[3] = {0,1,2};
  int *a = &t[1], *b = &u, *c = &v;
  int **x = &a, **y = &b, **z = &c;
  struct str_t {int *fst; int *snd; } s = { c , t }, *s1 = &s, *s2 = &s;
  c = jfla(s1->fst, s1->snd, x, y, 0);
  a = jfla(s2->fst, s2->snd, y, z, 1);
}
