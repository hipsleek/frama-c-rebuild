/* Loops over the heap — where the separation-logic backend earns its keep.
 *
 * Uses the same encoding as ll.c: a C `node* x` is the wrapper `node_star`
 * (a cell whose `pdata` field is the `node`), so `x` owns two cells —
 * `x::node_star<p> * p::node<...>` — and `x->val` is `x.pdata.val` in the .ss.
 *
 * Both functions verify SUCCESS.
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

/* Write to the same cell on every iteration. The point is the heap footprint:
   the loop spec must hand back the cells it borrowed, or the caller could not
   state `ensures x::node_star<p> * p::node<_,q>` after the loop. The node's
   value is left as `_` because the loop overwrites it. */
/*[SL]
  requires x::node_star<p> * p::node<_,q>
  ensures x::node_star<p> * p::node<_,q>;
*/
void bump(node* x, int k) {
  int i = 0;
  /*[SL_loop]
     requires x::node_star<p> * p::node<_,q>
     ensures x::node_star<p> * p::node<_,q> & i' >= k;
  */
  while (i < k) {
    x->val = i;
    i = i + 1;
  }
}

/* Walk a list and count it. This is a CONSUMING traversal: the loop spec takes
   `x::ll<k>` and its `ensures` says nothing about the heap, so the list is not
   handed back — which is exactly why `length` can only promise `res = n` and
   not `x::ll<n>`. Returning the list would need a list-segment predicate to
   describe the already-walked prefix, plus a lemma folding that segment back
   onto the tail; both are out of the current demo subset.

   The interesting part is that `k` is the length of whatever remains at loop
   entry, and the loop relates it to the counter: `c' = c + k`. Because the
   function enters with `c = 0` and `k = n`, that yields `res = n`. */
/*[SL]
  requires x::ll<n>
  ensures res = n;
*/
int length(node* x) {
  int c = 0;
  /*[SL_loop]
     requires x::ll<k>
     ensures c' = c + k;
  */
  while (x != (node*)0) {
    x = x->next;
    c = c + 1;
  }
  return c;
}
