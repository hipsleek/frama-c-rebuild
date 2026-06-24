/* C / Frama-C version of demo_hipsleek/hipexm.ss
 *
 * Parameters are real C pointers (`node* x`), so this is faithful C: the update
 * actually mutates the caller's node. The plugin encodes a C pointer `node*` as
 * the wrapper type `node_star` (a cell whose `pdata` field is the `node`), so a
 * `node* x` owns *two* cells — the pointer cell `x::node_star<p>` and the node
 * it points at `p::node<val,next>` — and `x->val` becomes `x.pdata.val` in the
 * generated .ss. The specs are written in that generated-.ss vocabulary.
 *
 * Two functions, the two basic heap effects:
 *   - get_val : READ   — returns the field, heap unchanged (res = v)
 *   - set_val : UPDATE — writes the field, leaving `next` (q) untouched
 * Both verify (SUCCESS).
 */

typedef struct node {
  int val;
  struct node* next;
} node;

/* READ: result equals the stored value; heap is preserved. */
/*[SL]
  requires x::node_star<p> * p::node<v,_>
  ensures x::node_star<p> * p::node<v,_> & res = v;
*/
int get_val(node* x) {
  return x->val;
}

/* UPDATE: val becomes w; the next field (q) is unchanged. */
/*[SL]
  requires x::node_star<p> * p::node<_,q>
  ensures x::node_star<p> * p::node<w,q>;
*/
void set_val(node* x, int w) {
  x->val = w;
}
