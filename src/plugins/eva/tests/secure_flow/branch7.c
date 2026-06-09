/*  run.config*
    COMMENT: Test function side-effect analysis with structs.
*/

struct foo {
    int x;
    int y;
};

/*@ requires \valid(p);
    assigns p->x;
 */
void write_x(struct foo *p, int a) {
    p->x = a;
}

/*@ requires \valid(p);
    assigns p->y;
 */
void write_y(struct foo *p, int b) {
    p->y = b;
}

extern int __fc_private secret;

int main(void) {
    struct foo s = { 1, 2 };
    /*@ assert security_status(s.x) == public; */
    /*@ assert security_status(s.x) == public; */
    if (secret) {
        write_x(&s, 3);
    } else {
        write_y(&s, 4);
    }
    /*@ assert security_status(s.x) == private; */
    /*@ assert security_status(s.y) == private; */

    /* Also test statement assigns annotations. */
    s.x = 0;
    /*@ assert security_status(s.x) == public; */
    if (secret) {
        /*@ assigns s.x; */
        s.x = 1;
    } else {
        /*@ assigns \nothing; */
        __asm__ ("nop");
    }
    /*@ assert security_status(s.x) == private; */

    return 0;
}
