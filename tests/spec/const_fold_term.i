/* run.config
  MODULE: @PTEST_NAME@
  STDOPT: -print
*/

void unop(int a) {
  //@ assert -(21 + 21);
  //@ assert ~21;
  //@ assert -a;
}

void binop(int a) {
  //@ assert 21 + 21 + a == 42;
  //@ assert 84 - 42 == 42;
  //@ assert 6 * 7 == 42;
  //@ assert 21 << 1 == 42;
  //@ assert 672 >> sizeof(int) == 42;
  //@ assert (58 & 47) == 42;
  //@ assert (34 | sizeof("frama-c")) == 42;
  //@ assert (63 ^ 21) == 42;
  //@ assert 168 / sizeof(a) == 42;
}
