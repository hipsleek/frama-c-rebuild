/*  run.config*
    COMMENT: Test security annotations in contracts for external functions.
*/

extern unsigned int __fc_private a;
extern unsigned int b;

/*@ assigns b \from a; */
extern void assign_b(void);

int main(void) {
    /*@ assert security_status(b) == public; */
    assign_b();
    // We have no definition of assign_b, but its assigns annotation allows
    // us to conclude the following:
    /*@ assert security_status(b) == private; */
    return 0;
}
