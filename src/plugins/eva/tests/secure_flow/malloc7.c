/* run.config*
   COMMENT: Test realloc
   STDOPT: +"-eva-verbose 0"
 */

#include <stdlib.h>

int main(void) {
    float **p, **q;

    // It's OK to call [realloc] with a null pointer as the first argument,
    // in which case it behaves like [malloc] for the given size.
 // p = malloc(10 * sizeof *p);
    p = realloc(NULL, 10 * sizeof *p);

    q = realloc(p, 5 * sizeof *q);

    // Calling [realloc] with a size of 0 is allowed in C and is equivalent
    // to calling [free]. We do not support it yet because it would mean
    // adding even more special cases to the complex code handling dynamic
    // allocations.
 // realloc(q, 0);
    free(q);

    /*@ assert security_status(q) == public; */

    return 0;
}
