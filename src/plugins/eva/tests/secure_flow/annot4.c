/*  run.config*
    COMMENT: Test security annotations in function contracts.
*/

/*@ requires security_status(x) == private && security_status(y) == public;
    ensures  security_status(\result) == private;
    assigns \result;
 */
int f(unsigned int x, unsigned int y) {
    int result = x + y;
    /*@ assert security_status(result) == security_status(x); */
    /*@ assert security_status(result) == private; */
    return result;
}

extern unsigned int __fc_private a;
extern unsigned int b;

int main(void) {
    return f(a, b);
}
