/*  run.config*
*/

int a;
int *b;
int *c = &a;
int **d = &c;

void main(){
    b = c;
    /*@ assert security_status(c) == public; */
}
