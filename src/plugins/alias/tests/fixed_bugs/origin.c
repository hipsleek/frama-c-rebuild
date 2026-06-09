/* run.config
   COMMENT: led to unhandled exception; extract from tests/value/origin.i
*/

struct {
  int p, t[];
} v;
void f() { int g = f ?: *(char *)v.t; }
