/*  run.config*
    COMMENT: Test security flow annotations on some types of declarations
*/

/* Test basic annotations. We want to support these:
 * - annotations public, private on variable declarations
 * - logic function security_status in annotations
 */

extern int __fc_public a;
extern int __fc_private b;
extern int /* security level not specified explicitly */ c;

int main(void) {
    /*@ assert security_status(a) == public; */
    /*@ assert security_status(b) == private; */
    return c;
}
