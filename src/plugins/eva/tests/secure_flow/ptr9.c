/*  run.config*
    COMMENT: Test arithmetic on dereference expressions
*/

void deref(unsigned int *p) {
    // Used to fail with error "not a MinusPP operation on pointers" due to
    // a spurious condition.
    *p += 42;
}

extern unsigned int __fc_private a;
extern unsigned int b;

int main(void) {
    deref(&a);
    deref(&b);
    return 0;
}
