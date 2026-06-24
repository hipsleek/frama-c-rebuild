/* Singly linked lists — Frama-C / HipSleek version of demo_hipsleek/ll.ss
 *
 * The `ll<n>` view tracks list length n. Because a C `node*` is encoded by the
 * plugin as the wrapper `node_star` (cell whose `pdata` is the `node`), the view
 * is written over that encoding: the pointer cell `self::node_star<p>`, the node
 * `p::node<_,q>`, and the recursive tail `q::ll<n-1>` (q is the `next` pointer,
 * itself a node*). Field access `x->next` becomes `x.pdata.next` in the .ss.
 *
 * Representative functions taken from ll.ss (the ones that stay within the C
 * subset — no `new node(...)` allocation, no `x.next.next` nested deref, no @R
 * reference parameters):
 *   get_next : detach and return the tail (split into ll<1> and ll<n-1>)
 *   set_next : set the tail pointer, length becomes j+1
 *   set_null : truncate to a single node (length 1)
 *   append   : in-place append, length n1+n2 (recursive; kept last)
 * All verify (SUCCESS).
 */

/*[SL_pred]
ll<n> == self = null & n = 0
  or self::node_star<p> * p::node<_,q> * q::ll<n-1>
  inv n >= 0;
*/

typedef struct node {
  int val;
  struct node* next;
} node;

/* detach the tail: x keeps its head (length 1), the tail is returned (length n-1). */
/*[SL]
  requires x::ll<n> & n > 0
  ensures x::ll<1> * res::ll<n-1>;
*/
node* get_next(node* x) {
  node* tmp = x->next;
  x->next = (node*)0;
  return tmp;
}

/* set the tail of x to y; new length is j+1. */
/*[SL]
  requires x::ll<i> * y::ll<j> & i > 0
  ensures x::ll<j+1>;
*/
void set_next(node* x, node* y) {
  x->next = y;
}

/* truncate x to a single node. */
/*[SL]
  requires x::ll<i> & i > 0
  ensures x::ll<1>;
*/
void set_null(node* x) {
  x->next = (node*)0;
}

/* append two singly linked lists in place; lengths add up. (recursive) */
/*[SL]
  requires x::ll<n1> * y::ll<n2> & x != null
  ensures x::ll<n1+n2>;
*/
void append(node* x, node* y) {
  if (x->next == 0)
    x->next = y;
  else
    append(x->next, y);
}
