/*  run.config*
*/

int a;
int b;
int *c = &a;
int *d = &b;
void main(){

    *c += *d;
    /*@ assert security_status(c) == public; */
}
