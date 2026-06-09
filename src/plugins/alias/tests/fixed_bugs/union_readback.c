// REPRODUCE: frama-c -alias -alias-verbose 2 union_readback.c
// BEHAVIOUR: fatal kernel error [kernel] test.c:10: Failure: 
//   typeOffset: Field r on a non-compound type 'char const **'
// EXPECTED: no error
// EXPLANATION:
//   This happens during the readback phase while reconstructing lvals.
//   The call [Cil.typeOfLval lv] on the reconstructed lval [lv] fails.
// FIX: take into account typing information during reconstruction.

typedef struct s {
    union {
        const char **v;
    } r;
} s;

void f(const char** o) {
    s flag;
    flag.r.v = o;
}
