const int t[4] = { 4, 2 };

const struct { int i1; int i2;} u[3] = { 2, 5, 3, 7};

struct s {
  int i;
  int j ;
  struct v { int k; int l;} v;
};

const struct s s = { 5, 8, 3 };

struct T;

volatile int c;

void main() {
  unsigned int i = 0;

  //@ loop unfold sizeof(t)/sizeof(t[0]); // 4
  while (c) {
    i++;
  }

  //@ loop unfold \offset(&s.v.l); // 12
  while (c) {
    i++;
  }

  //@ loop unfold s.i + s.v.l; // 5+0
  while (c) {
    i++;
  }


  //@ loop unfold \max(t[..]); // 4
  while (c) {
    i++;
  }

  //@ loop unfold \min(t[..]); // 0 because of missing initializer
  while (c) {
    i++;
  }

  //@ loop unfold \max(\union(1, 1+s.i)); // 6
  while (c) {
    i++;
  }


  //@ loop unfold \min(t[\union(1, 3)]) + \max(t[\union(1, 3)]); // 2+0
  while (c) {
    i++;
  }

  //@ loop unfold \min(u[..1].i1) * \max(u[..1].i1); // 2*3
  while (c) {
    i++;
  }



}
