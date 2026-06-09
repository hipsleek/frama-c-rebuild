#include <stdio.h>
void main() {
  strlen("");
}
struct s {
  int a; int b;
} s;

volatile int v;

int fn2(int, int);

int fn1(int x, int y)
{
  Frama_C_show_each_1(x);
  Frama_C_show_each_2(y);
  return x + y;
}

int X, Y;
int main1 () {
  R1 = fn1(G, G|0);
  R2 = fn2(G, G|0);
  Frama_C_show_each_d(G);
  pv = (int *) &X;
  return Y;
}

int * main2() {
  return 0;
}

#define not_a_function_call(v)                  \
  yes_a_function_call(v)

#define yet_another_not_a_call(v) do {          \
    yes_again();                                \
  } while (0)

/* call_inside_comment(evaluation); */
void main3 () {
  //@ not_a_function_call(v);
  yet_another_not_a_call(v);
}

/* Tests the initialization of local variables. */
void main4 () {
  f(g(h(i, j), k (l, m  (  n(
                             o, p(
                                  q)
                             )
                           )
                  )
      )
    );
  f();
  g();
}
void main() {
  main1();
  main2();
  main3();
  main4();
}
