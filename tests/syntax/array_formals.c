/* run.config
  STDOPT:
  EXIT: 1
    STDOPT: +"-cpp-extra-args=-DWRONG_RESTRICT"
*/

int f(int a[2]) { return a[1]; }

int g(int a[static 2]) { return a[1]; }

int h(int a[static restrict const 2][2]) { return a[1][1]; }

typedef int (__attribute__((test)) arr)[2];

int k(arr a) { return a[1]; }

int l() {
  arr a = { 0 };
  return k(a);
}

#ifdef WRONG_RESTRICT
  // Restrict qualifier can only be used in the first dimension of an array
  int m(int a[2][restrict 3]) { return a[0][1]; }
#endif
