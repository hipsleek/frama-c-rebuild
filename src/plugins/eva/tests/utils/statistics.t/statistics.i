void g(int i) {}

void f(int n) {
  for (int i = 0 ; i < n ; i++) {
    g(i);
  }
}

void h() {
  // Test loop unrolling statistics, it should output 4 and 5
  //@ loop unroll 100;
  for (int i = 0 ; i < 4 ; i++) {
    //@ loop unroll 100;
    for (int j = 0 ; j < 5 ; j++) {
      // Do nothing
    }
  }
}

int main(int n) {
  f(n);
  f(n-1);
  h();
}
