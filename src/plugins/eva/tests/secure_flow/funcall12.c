/*  run.config*
    COMMENT: Test multi-target indirect function calls
*/

extern int global_x;
extern int global_y;

int func_a(int a) {
    return a;
}

int func_b(int b) {
    global_x = b;
    return b;
}

/*@ assigns \result \from c, global_y; */
int func_c(int c); // no definition

extern int __fc_private secret;

int main(void) {
    int (*ptr)(int) = (secret < 0 ? func_a : secret == 0 ? func_b : func_c);
    // The following assert is true but the taint domain does not treat function
    // pointers as expected at the moment.
    /*@ assert security_status(ptr) == private; */

    int arg = 42;
    int result = ptr(arg);
    /*@ assert security_status(result) == private; */

    // One of the possible targets of the indirect call is [func_b], which
    // writes to the [global_x] variable.
    /*@ assert security_status(global_x) == private; */

    return result;
}
