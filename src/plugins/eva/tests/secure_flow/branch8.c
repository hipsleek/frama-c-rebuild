/*  run.config*
    COMMENT: Test side effect analysis with contracts on complex statements.
*/

extern int __fc_private secret;

int main(void) {
    int x, y;

    /*@ assigns x, y; */
    if (secret) {
        x = 1;
    } else {
        y = 1;
    }

    /*@ assigns x, y; */
    switch (secret) {
    case 0:
        x = 2;
        break;
    default:
        y = 2;
        break;
    }

    return 0;
}
