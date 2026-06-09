/*  run.config*
    COMMENT: Test information flow for function return values.
*/

unsigned int add(unsigned int x, unsigned int y);

extern unsigned int __fc_private a;
extern unsigned int b;

int main(void) {
    unsigned int sum = add(a, b);
    /*@ assert security_status(sum) == security_status(a) ||
               security_status(sum) == security_status(b);
     */
    return 0;
}

unsigned int add(unsigned int x, unsigned int y) {
    return x + y;
}
