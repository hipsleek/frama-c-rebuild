/* run.config*
   COMMENT: Test basic malloc and calloc calls
   STDOPT: +"-eva-verbose 0 -eva-no-alloc-returns-null" #"-eva-no-alloc-returns-null"
 */
#include <stdlib.h>

struct foo {
    int a;
    double b;
};

extern double __fc_private secret;

int main(void) {
    char *p = malloc(2 * sizeof (char));
    p[0] = 'a';
    p[1] = 'b';

    float *q = calloc(42, sizeof (double));
    *q = 42.0;

    struct foo *r = malloc(sizeof *r);
    r->b = secret;

    return 0;
}
