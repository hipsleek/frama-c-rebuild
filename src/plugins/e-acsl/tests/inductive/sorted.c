/* run.config
   COMMENT: based on ACSL Mini-Tutorial (https://github.com/acsl-language/acsl)
   STDOPT: +"-eva-unroll-recursive-calls 9"
*/

#include <stddef.h>

typedef struct list {
  int hd;
  struct list *next;
} list;

/*@
  inductive sorted_decr(list* node) {
    case sorted_nil: sorted_decr(\null);
    case sorted_singleton:
    \forall list* node;
       \valid(node) && node->next == \null ==>
          sorted_decr(node);
    case sorted_next:
    \forall list* node;
       \valid(node) && \valid(node->next) &&
       node->hd >= node->next->hd &&
          sorted_decr(node->next) ==> sorted_decr(node);
  }
*/

list last = {.hd = 2, .next = NULL};
list next = {.hd = 3, .next = &last};
list root = {.hd = 1, .next = &next};

int main() {
  //@ assert sorted_decr(&next);
  //@ assert !sorted_decr(&root);
  return 0;
}
