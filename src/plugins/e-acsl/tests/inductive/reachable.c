/* run.config
   COMMENT: reachable based on example from the ACSL specification
   COMMENT: same_elements is from src/plugins/wp/tests/wp_plugin/inductive.i
   STDOPT: +"-eva-unroll-recursive-calls 9"
*/

#include <stddef.h>

typedef struct list {
  int hd;
  struct list *next;
} list;

// reachability in linked lists
/*@
  inductive reachable(struct list *root, struct list *to) {
      case empty: \forall struct list *l; reachable(l,l);
      case non_empty: \forall struct list *l1,*l2;
          \valid(l1) && reachable(l1->next,l2) ==> reachable(l1,l2);
}
*/

// not translatable, because of same_elements's case swap; it cannot bind i, j.
/*@
  predicate swap(int *a, int *b, integer begin, integer i, integer j, integer end) =
       begin <= i < j < end &&
       a[i] == b[j] &&
       a[j] == b[i] &&
       \forall integer k; begin <= k < end && k != i && k != j ==>
       a[k] == b[k];

  predicate same_array(int *a, int *b, integer begin, integer end) =
    \forall integer k; begin <= k < end ==> a[k] == b[k];

  inductive same_elements(int *a, int *b, integer begin, integer end) {
    case refl:
      \forall int *a, int *b, integer begin, end;
      same_array(a, b, begin, end) ==>
      same_elements(a, b, begin, end);
    case swap: \forall int *a, int *b, integer begin, i, j, end;
      swap(a, b, begin, i, j, end) ==>
      same_elements(a, b, begin, end);
    case trans: \forall int* a, int *b, int *c, integer begin, end;
      same_elements(a, b, begin, end) ==>
      same_elements(b, c, begin, end) ==>
      same_elements(a, c, begin, end);
  }
*/

list last = {.hd = 3, .next = NULL};
list next = {.hd = 2, .next = &last};
list root = {.hd = 1, .next = &next};

int main() {
  // strange: without this the next assertion fails
  //@ assert \valid(&root);

  //@ assert reachable(&root, &next);
  //@ assert !reachable(&next, &root);

  int arr[3] = {1, 2, 3};
  //@ assert same_elements((int*)arr, (int*)arr, 0, 1);
  return 0;
}
