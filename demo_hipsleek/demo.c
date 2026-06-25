//Simple example with heap updates and reads

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


// heap access with aliasing


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



//SL predicates and recursion

/*[SL_pred]
ll<n> == self = null & n = 0
  or self::node_star<p> * p::node<_,q> * q::ll<n-1>
  inv n >= 0;
*/



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
