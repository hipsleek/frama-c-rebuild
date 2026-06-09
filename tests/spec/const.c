//@ logic integer strlen(char* c);
//@ predicate sreads(char *c) reads c[0..];
//@ requires strlen(c) < n; ensures strlen(a) <=n;
void f(const char* c, char* restrict a, int n) {

}
