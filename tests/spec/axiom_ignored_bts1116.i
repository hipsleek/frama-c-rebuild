
/*@ axiom l: // no longer rejected; axioms allowed outside an axiomatic
    \forall int i; i < 0;
 */

struct _str {
    int x;
};

//@ ensures \result < 0;
int ftest(int i) {
    return i;
}

