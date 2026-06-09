/* run.config
COMMENT: [e-acsl] Failure: typing was not performed on construct s in phase `analysis:typing'
*/

/*@
  axiomatic p {
    predicate p(ℤ s);
  }
*/

/*@ ensures p(s); */
void f(int s) {
  return;
}

int main() {
  f(2);
}

/*@

axiomatic g {
  logic ℤ g(int c)
    reads c;
}

@*/

/*@ ensures g(t) == 0; */
int h(const int t) {
  return 0;
}

int i() {
  h(1);
}
