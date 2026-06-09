/* run.config
   STDOPT:
*/

struct foo { char bar[4]; };

// Long names to check Format indentation
int const aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa = 0;
int const bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb = 3;

/*@ assigns x->bar[name_range1:(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa..bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)]
      \from x->bar[aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa..bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb];
*/
int f(struct foo* x);

void main() {
  int a = 0;
  //@ assert \subset(a, (0..1));
  //@ assert \subset(a, (name_range2:(0..2)));
}
