/*  run.config*
    COMMENT: Test information flow for function return values (struct types).
*/

struct addition {
    unsigned int op_a, op_b, result;
};

struct addition add(unsigned int x, unsigned int y);

extern unsigned int __fc_private a;
extern unsigned int b;

int main(void) {
    struct addition sum = add(a, b);
    /*@ assert security_status(sum.op_a) == security_status(a) == private; */
    /*@ assert security_status(sum.op_b) == security_status(b) == public; */
    /*@ assert security_status(sum.result) == private; */
    return 0;
}

struct addition add(unsigned int x, unsigned int y) {
    struct addition r = { x, y, x + y };
    return r;
}
