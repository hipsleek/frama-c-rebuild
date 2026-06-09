/*  run.config*
    COMMENT: Test side-effect analysis of sensitive updates and gotos.
*/

extern int __fc_private secret;
extern unsigned int one, two, three;

int compute(int n) {
    if (secret) {
        one++;
        return two;
    } else {
        three++;
    }
    return 0;
}

int compute2(int n) {
    switch (!!secret) {
    case 1:
        one++;
        return two;
    default:
        three++;
    }
    return 0;
}

int compute3(int n) {
    if (secret) {
        one++;
        return one;
    } else {
        three++;
        return three;
    }
    return 0;
}

int compute4(int n) {
    switch (!!secret) {
    case 1:
        one++;
        goto end;
    default:
        three++;
        goto end;
    }
end:
    return 0;
}

int main(void) {
    compute(one);
    /*@ assert security_status(one) == private; */
    /*@ assert security_status(two) == public; */
    /*@ assert security_status(three) == private; */

    one = three = 0;
    compute2(two);
    /*@ assert security_status(one) == private; */
    /*@ assert security_status(two) == public; */
    /*@ assert security_status(three) == private; */

    one = three = 0;
    compute3(two);
    /*@ assert security_status(one) == private; */
    /*@ assert security_status(two) == public; */
    /*@ assert security_status(three) == private; */

    one = three = 0;
    compute4(two);
    /*@ assert security_status(one) == private; */
    /*@ assert security_status(two) == public; */
    /*@ assert security_status(three) == private; */

    return 0;
}
