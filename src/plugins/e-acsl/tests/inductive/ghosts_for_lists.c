/* run.config
   COMMENT: from the paper Ghosts for Lists: from Axiomatic to Executable Specifications
   COMMENT:
   COMMENT: This predicate inductively relates a list starting at root to a segment of com-
   COMMENT: panion array cArr, starting from an offset index and having n elements, that
   COMMENT: ends with the excluded cell address bound (either NULL or a pointer to the
   COMMENT: first non-represented list element if any). This relation is verified (cf. axiom
   COMMENT: linked_n_cons) if root is a valid memory location, if we find this
   COMMENT: value at offset index of cArr, and if, recursively, the list that starts at root->next
   COMMENT: is linked to the segment starting from index+1 with n-1 elements. That is, for
   COMMENT: all i, the address of the ith cell of the list can be found at index+i of cArr.
   STDOPT: +"-eva-unroll-recursive-calls 9"
*/

#include <stdlib.h>

#define MAX_SIZE 10

struct list {
  struct list *next;
  int value;
};

/*@
inductive linked_n{L}(struct list *root, struct list **cArr, ℤ index, ℤ n, struct list *bound) {
  case linked_n_bound{L}:
    ∀ struct list **cArr, *bound, ℤ index ;
      0 ≤ index ≤ MAX_SIZE ⇒
      linked_n (bound, cArr, index, 0, bound);
  case linked_n_cons{L}:
    ∀ struct list *root, **cArr, *bound, ℤ index, n ;
    0 < n ∧ 0 ≤ index ∧ 0 ≤ index + n ≤ MAX_SIZE ∧
    \valid (root) ∧ root == cArr[index] ∧
    linked_n (root->next, cArr, index + 1, n - 1, bound) ⇒
    linked_n (root, cArr, index, n, bound);
}
*/

int main() {
  /*@ assert linked_n(NULL, NULL, 0, 0, NULL); */
  struct list leaf = {NULL, 1};
  struct list root = {&leaf, 2};
  struct list *cArr[2] = {&root, &leaf};
  /*@ assert linked_n(&root, &cArr[0], 0, 2, NULL); */
  struct list *cArr2[2] = {&leaf, &root};
  /*@ assert !linked_n(&root, &cArr2[0], 0, 2, NULL); */
  return 0;
}
