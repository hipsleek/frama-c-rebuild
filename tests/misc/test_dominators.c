/* run.config
  MODULE: @PTEST_NAME@
  LOG: postdom_graph.f.dot dom_graph.f.dot
  LOG: postdom_graph.g.dot dom_graph.g.dot
  LOG: postdom_graph.h.dot dom_graph.h.dot
  LOG: postdom_graph.i.dot dom_graph.i.dot
  OPT: -kernel-msg-key printer:sid -print-as-is
*/

void stop(void) __attribute__ ((noreturn)) ;

int f (int c){
  c = 12;
  if (c) {
    test: c = 42;
    goto test;
  }
  else {
    c = 12;
    return c;
  }
}

int g (int c){
  c = 12;
  if (c) {
    goto test;
    c++;
    test: c = 42;
  }
  else {
    c = 12;
    return c;
  }
  return c;
}

int h(int x){
  for (int j = 0; j < 10; j++){
    if (j % 2 == 0) x++;
  }
  stop();
  return x;
}

void i (int nondet) {
  int x = 0;
  if (nondet) goto loop;
  x = 1;
  while (x < 10) {
    x++;
    loop: ;
  }
}
