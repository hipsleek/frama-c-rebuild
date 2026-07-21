/* Immutability annotation @L ("Lend") — a HipSleek feature.
 *
 * A heap assertion `p::node<v,_>` in a `requires` normally CONSUMES the node:
 * the callee takes ownership, and unless the `ensures` hands it back the caller
 * has lost it. `@L` changes that to a READ-ONLY BORROW ("lend"): the callee may
 * read the node but not mutate it, and the node is returned to the caller
 * automatically — so the `ensures` need not (and must not) re-state it.
 *
 * The plugin encodes a C pointer `node* x` as the wrapper `node_star` — the
 * pointer cell `x::node_star<p>` plus the pointed-at node `p::node<val,next>` —
 * and `x->val` becomes `x.pdata.val` in the generated .ss. So the node being
 * lent is `p`, and `@L` is written on `p::node<v,_>@L`.
 *
 *   get_val   : reads x->val under @L — ensures returns res = v only; the node
 *               p is lent, so it is NOT restated in the postcondition.
 *   double_val: the payoff — calls get_val(x) TWICE and still owns the node
 *               afterwards (ensures restates the full node). This type-checks
 *               ONLY because @L lends rather than consumes: a plain (mutable)
 *               borrow would be gone after the first call and the second
 *               get_val(x) would have no node to read (see loop_bad-style FAIL).
 * Both verify SUCCESS.
 */

typedef struct node {
  int val;
  struct node* next;
} node;

/* READ under @L: the node p is lent, not consumed. The postcondition mentions
   only the pointer cell (never lent) and the result — p returns automatically. */
/*[SL]
  requires x::node_star<p> * p::node<v,_>@L
  ensures x::node_star<p> & res = v;
*/
int get_val(node* x) {
  return x->val;
}

/* PAYOFF: two reads of the same node, and the caller keeps the whole node.
   Provable only because get_val merely lends x — a consuming borrow would make
   the second get_val(x) fail with the node already given away. */
/*[SL]
  requires x::node_star<p> * p::node<v,_>
  ensures x::node_star<p> * p::node<v,_> & res = v + v;
*/
int double_val(node* x) {
  return get_val(x) + get_val(x);
}

/* READ: result equals the stored value; heap is preserved. */
/*[SL]
  requires x::node_star<p>@L * p::node<v,_>@L
  ensures  res = v;
*/
int get_val2(node* x) {
  return x->val;
}