/*  run.config*
    COMMENT: Test side-effect analysis of multiple jumps to same target.
*/

extern int __fc_private secret;
extern int one, two, three;

int compute(int n) {
    switch (n) {
    case 1:
        one = 1;
        if (secret) return two;
        break;
    case 2:
        two = 2;
        if (secret) goto my_return_label;
        break;
    case 3:
        three = 3;
        if (secret) return one;
        break;
    default:
        if (secret) goto my_return_label;
        break;
    }

my_return_label:
    return n;
}

int main(void) {
    int result;
    /*@ assert security_status(result) == public; */
    result = compute(one);
    /*@ assert security_status(result) == private; */
    return result;
}
