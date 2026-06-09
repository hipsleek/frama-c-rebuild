/*  run.config*
    COMMENT: Test information flow for function return values (pointers).
*/

unsigned int internal_result = 0;
/*@ requires \valid(x) && \valid(y);
    ensures  \valid(\result);
    assigns  internal_result \from *x, *y;
 */
unsigned int *add(unsigned int *x, unsigned int *y);

extern unsigned int __fc_private a;
extern unsigned int b;

int main(void) {
    unsigned int *sum = add(&a, &b);
    /*@ assert security_status(*sum) == private; */
    return 0;
}

unsigned int *add(unsigned int *x, unsigned int *y) {
    internal_result = *x + *y;
    return &internal_result;
}
