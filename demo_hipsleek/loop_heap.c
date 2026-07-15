/* Loops over the heap — where the separation-logic backend earns its keep.
 *
 * Uses the same encoding as ll.c: a C `node* x` is the wrapper `node_star`
 * (a cell whose `pdata` field is the `node`), so `x` owns two cells —
 * `x::node_star<p> * p::node<...>` — and `x->val` is `x.pdata.val` in the .ss.
 *
 * All four functions verify SUCCESS. The pair `walk_destructive` / `length`
 * is the point of this file: they are the same algorithm, and the only
 * difference is whether the loop clobbers the parameter or a cursor.
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

/* CONSUMING traversal — the cautionary version. The loop walks `x` itself, so
   on exit `x` is null and the `ensures` has nothing left to say about the
   list: it is not handed back, and the caller cannot state `x::ll<n>`
   afterwards. The count is still provable, so this verifies — it just proves
   much less than you probably wanted. Compare `length` below. */
/*[SL]
  requires x::ll<n>
  ensures res = n;
*/
int walk_destructive(node* x) {
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

/* NON-CONSUMING traversal — the version you want. Walking a separate cursor
   leaves the parameter `x` pointing at the head, so the loop can give the list
   back: `ensures cur::ll<k>` names the entry value of `cur`, which is `x`.
   HipSleek re-folds the list on the way out of the recursion (each iteration
   unfolds one node and folds it back), so no list-segment predicate or lemma
   is needed here — just a cursor. That is what lets the function promise
   `x::ll<n> & res = n` rather than only `res = n`. */
/*[SL]
  requires x::ll<n>
  ensures x::ll<n> & res = n;
*/
int length(node* x) {
  int c = 0;
  node* cur = x;
  /*[SL_loop]
     requires cur::ll<k>
     ensures cur::ll<k> & cur' = null & c' = c + k;
  */
  while (cur != (node*)0) {
    cur = cur->next;
    c = c + 1;
  }
  return c;
}

/* Mutating every node in a loop while preserving the list's shape. The `ll`
   view leaves the value field as `_`, so overwriting it keeps the list a list:
   the loop borrows `cur::ll<k>`, rewrites each node, and folds `ll<k>` back. */
/*[SL]
  requires x::ll<n>
  ensures x::ll<n>;
*/
void zero_all(node* x) {
  node* cur = x;
  /*[SL_loop]
     requires cur::ll<k>
     ensures cur::ll<k> & cur' = null;
  */
  while (cur != (node*)0) {
    cur->val = 0;
    cur = cur->next;
  }
}
