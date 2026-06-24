/* Aliasing in separation logic (Frama-C + HipSleek).
 *
 * As in hipexm.c, a C `node*` is encoded by the plugin as the wrapper type
 * `node_star` (a cell whose `pdata` field is the `node`), so `node* x` owns the
 * pointer cell `x::node_star<p>` plus the node `p::node<val,next>`, and `x->val`
 * becomes `x.pdata.val` in the generated .ss.
 *
 * Two complementary facets of aliasing:
 *
 *   alias_write : y is made to ALIAS x (y = x), so a write through x is observed
 *                 when reading through y. There is only one node cell `p`; both
 *                 x and y refer to it, so res == 5 after `x->val = 5`.
 *
 *   set_two     : the SEPARATING conjunction `*` in the precondition guarantees
 *                 x and y own DISJOINT cells (px != py) — i.e. NOT aliased — so
 *                 writing through x cannot disturb y's value, and vice versa.
 *
 * Both verify (SUCCESS).
 */

typedef struct node {
  int val;
  struct node* next;
} node;

/* Aliasing: write through x, read through its alias y -> sees the new value. */
/*[SL]
  requires x::node_star<p> * p::node<_,q>
  ensures x::node_star<p> * p::node<5,q> & res = 5;
*/
int alias_write(node* x) {
  node* y = x;     /* y now aliases x (same node cell) */
  x->val = 5;      /* write through x ...              */
  return y->val;   /* ... observed through y           */
}

/* Aliased INPUTS: the two parameters are the same pointer (x = y). Only one node
   cell is owned; a write through x is therefore seen when reading through y. */
/*[SL]
  requires x::node_star<p> * p::node<_,q> & x = y
  ensures x::node_star<p> * p::node<7,q> & res = 7;
*/
int aliased_inputs(node* x, node* y) {
  x->val = 7;      /* write through x ...            */
  return y->val;   /* ... seen through y, since y==x */
}

/* Separation = non-aliasing: x and y are disjoint, so the writes don't interfere. */
/*[SL]
  requires x::node_star<px> * px::node<_,_> * y::node_star<py> * py::node<_,_>
  ensures x::node_star<px> * px::node<a,_> * y::node_star<py> * py::node<b,_>;
*/
void set_two(node* x, node* y, int a, int b) {
  x->val = a;
  y->val = b;
}

/* FAILING case: calling set_two with the SAME pointer for both arguments.
   set_two's precondition `x::... * y::...` demands two DISJOINT cells, but a
   single node `x` cannot be split into two separated cells, so the call's
   precondition is unsatisfiable and verification FAILS (expected). This is how
   separation logic rejects unintended aliasing at a call site. */
/*[SL]
  requires x::node_star<p> * p::node<_,q>
  ensures x::node_star<p> * p::node<_,q>;
*/
void set_two_aliased(node* x) {
  set_two(x, x, 1, 2);   /* FAIL: needs x and y to be disjoint */
}
