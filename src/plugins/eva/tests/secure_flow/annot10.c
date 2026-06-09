/*  run.config*
    COMMENT: Test generation of non-interference on conditions
*/

extern int __fc_public a, b, c;
extern int __fc_private d;

int main(void) {
    int x, y, z;

    if (a && b) {
        x = 1;
    } else {
        x = 2;
    }

    while (c && x < 3) {
        x++;
    }

    switch (d) {
    case 0:
        y = 1;
        z = 2;
        break;
    case 1:
        z = 1;
        y = 2;
        break;
    default:
        y = 0;
        z = 0;
    }

    return x + y + z;
}
