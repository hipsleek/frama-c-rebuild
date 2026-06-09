/*  run.config*
    COMMENT: Test function side-effect analysis.
*/

void g(int g_a);
void h(int h_b);
int global_result;

void compute(int a, int b) {
    g(a);
    h(b);
    global_result = a + b;
}

int global_g;

void g(int g_a) {
    g_a += 10;
    global_g = g_a;
}

int global_h;
int *global_ptr = &global_h;

void h(int h_b) {
    h_b -= 10;
    *global_ptr = h_b;
}

extern int __fc_private secret;

int main(void) {
    if (secret) {
        compute(23, 42);
    }

    /* All of global_result, global_g, and global_h are secret because
     * they are modified by functions called from a secret context */
    /*@ assert security_status(global_result) == private; */
    /*@ assert security_status(global_g) == private; */
    /*@ assert security_status(global_h) == private; */

    /* global_ptr is still public. It was used for an assignment in a secret
     * context, but it was not itself assigned. */
    /*@ assert security_status(global_ptr) == public; */
}
