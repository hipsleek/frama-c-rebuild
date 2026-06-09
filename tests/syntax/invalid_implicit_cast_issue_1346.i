/* run.config*
 EXIT: 1
   STDOPT:
*/

char B[1];

typedef void *FILE; // error: incompatible type redefinition
typedef struct globals {
  FILE *l;
} T;

void f(FILE *file) {
  (*(T*)&B) = 0; // error: implicit cast: cannot cast from int to T
}
