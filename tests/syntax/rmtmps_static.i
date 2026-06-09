int f() {
  static int x = 0;
  return 0;
}

/* Tests that Rmtmps removes unused static or inline functions. */

static int x;
static int y;

static int static_incr_x(void) { return x++; } // called
static int static_incr_y(void) { return y++; } // unused

static int static_decr_x(void); // called
static int static_decr_y(void); // unused

inline int inline_incr_x (int x) { return x+1; } // called
inline int inline_incr_y (int y) { return y-1; } // unused

inline int inline_decr_x(int x); // called
inline int inline_decr_y(int y); // unused

void g() {
  int a;
  a = static_incr_x();
  a = static_decr_x();
  a = inline_incr_x(a);
  a = inline_decr_x(a);
}
