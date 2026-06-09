/*
  cf. issue frama-c/frama-c#1620: The generated code uses variables not (yet)
  declared.
*/

/*@ ghost
  void f1() {
    {
      int o = 0;
      o++;
    };
    return;
  }
*/

/*@ ghost
  void f2() {
    {
      int o = 0;
      int \ghost * p = &o;
      *p = 1;
    };
    return;
  }
*/

int main(void) {
  return 0;
}
