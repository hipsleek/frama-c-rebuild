/*  run.config*
*/

int x[10];

int *y;
int *z;

int n =10;

void main(){
    y = x;

    z = &n;

    *z = *( y + 5);
    /*@ assert security_status(*y) == public; */
}
