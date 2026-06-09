/*  run.config*
    COMMENT: Test information flow for function return values with pointers.
*/

unsigned int add(unsigned int x, unsigned int y);

extern unsigned int __fc_private a;
extern unsigned int b;

extern int __fc_private secret;

int main(void) {
    unsigned int sum;
    unsigned int *p = &sum;
    *p = add(a, b);
    /*@ assert security_status(sum) == private; */

    return 0;
}

unsigned int add(unsigned int x, unsigned int y) {
    return x + y;
}
