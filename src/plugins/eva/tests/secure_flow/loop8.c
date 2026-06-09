/*  run.config*
    COMMENT: Test transformation of loops without annotations.
    STDOPT: +"-eva-slevel 1"
*/

extern unsigned int __fc_private secret;
extern unsigned int user_input;

int main(void) {
    unsigned int i = 0u;
    int array[10] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

    for (i = 1u; i < 10u; i++) {
        array[i] = 1;
    }

    for (i = 1u; i < 10u; i += 3u) {
        if (secret) {
            array[i] = 2;
        }
    }

    return 0;
}
