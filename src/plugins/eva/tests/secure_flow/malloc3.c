/* run.config*
   COMMENT: Test dynamic allocation and branching
   STDOPT: #"-eva-slevel 3 -eva-no-alloc-returns-null" +"-eva-verbose 0 -eva-slevel 3 -eva-no-alloc-returns-null"
 */
#include <stdlib.h>

extern int __fc_private secret;

int main(void) {
    int *p = NULL, *q = NULL;

    if (secret < 0) {
        p = malloc(sizeof *p);
    } else if (secret > 0) {
        p = malloc(sizeof *p);
    } else {
        q = malloc(sizeof *q);
    }

    /*@ check security_status(p) == private; */
    /*@ check security_status(q) == private; */

    if (secret) {
        *p = 1;
    } else {
        *q = 2;
    }

    free(p);
    free(q);

    return 0;
}
