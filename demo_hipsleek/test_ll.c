/*[SL_pred]
ll<> == self = null
  or self::node_star<p> * p::node<_,q> * q::ll<>;
*/

typedef struct node {
  int val;
  struct node* next;
} node;

/*[SL]
  requires x::ll<>
  ensures x::ll<>;
*/
/*@ requires \valid(x) || x == \null;
    assigns \nothing;
*/
int length(node* x) {
  if (x == 0) return 0;
  return 1 + length(x->next);
}

/*[SL]
  requires x::ll<> * y::ll<>
  ensures res::ll<>;
*/
/*@ requires \valid(x) || x == \null;
    requires \valid(y) || y == \null;
    assigns \nothing;
*/
node* append(node* x, node* y) {
  if (x == 0) return y;
  x->next = append(x->next, y);
  return x;
}
