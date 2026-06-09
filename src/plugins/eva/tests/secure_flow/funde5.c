/*  run.config*
*/

int a;
int *b;

void f(int s, int *r);

void f(int s, int *t){
    a+=1 ;
    /*@ assert security_status(*t) == public; */
}

void main(){
    b = &a;
    f(a,b);
}
