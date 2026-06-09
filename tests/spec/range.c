/* run.config
OPT: -print -ocode @PTEST_NAME@_reparse.c -then @PTEST_NAME@_reparse.c -print -ocode=""
*/

//@ logic set<integer>range(integer x, integer y) = (x .. y);

struct foo { char bar[4]; };

/*@ assigns x->bar[0..3] \from x->bar[0..3]; */
int f(struct foo* x);

typedef char baz[4];

struct bli { baz bli; };

/*@ assigns x[0..3] \from y->bli[0..3]; */
int g(baz x,struct bli* y);

/*@ assigns *(x + (0 .. ((1 << 2) - 2))) \from \let range = (0 .. 4-1); x[range];
    ensures \let range = (0 .. 4 - 1); x[range] == 0;
 */
int h(baz x);
