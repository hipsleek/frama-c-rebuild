/* run.config
   STDOPT:
*/

struct X {
     int arr[2];
};

struct S {
     struct X x;
};


struct X get_x(void) { return (struct X){42,16}; }

struct S get_s(void) { return (struct S){ (struct X){42,16} }; }

int init(void) {
    int *p = get_x().arr; // OK
    p = get_x().arr; // OK
    int const *q = get_x().arr; // OK
    return *p + *q; // UB
}

int g1(int* x) { return *x; }
void g2(int* x) { *x = 1; }

//from issue 1220
int* calls(void) {
    // C99: UB access to a[0] in g1 whose lifetime ended
    //      at the sequence point at the start of g1
    // C11: OK, d is 2.0
    int d = g1(get_x().arr); // OK

    // C99: UB modification of a[0] whose lifetime ended at the sequence point
    // C11: UB attempt to modify a temporary object
    g2(get_x().arr); // OK

    return get_x().arr; // OK
}

int* cond(void) {
    int *p;

    if(get_x().arr);
    if((char*)get_x().arr) return (int*)1; // OK
    if(*(get_x().arr + 0)) return (int*)2; // OK

    if(*(get_x().arr + 0) || (p = get_x().arr, *(p + 1))) return (int*)3; // OK

    for (int i = 0; get_x().arr; i++); // OK
    do{} while (*(get_x().arr + 0) && *(get_x().arr + 1)); // OK

    return p; // UB
}

int paren_return(void){

    get_x().arr; // OK
    (get_x().arr); // OK

    if((get_x().arr)) return 1; //OK

    return *(get_x().arr + 0); // OK
}

int nested(void){
    int *p = get_s().x.arr; //OK
    if((get_s().x.arr)) return 1; // OK
    int d = g1(get_s().x.arr); // OK
    return *p; // UB
}

int f() {return 1;}

int comma(void){
    int *p;
    int *q;
    int x = (p = get_x().arr, *p); //OK
    int y = *p; // UB

    if((p = get_x().arr, q = get_x().arr, *p + *q))  { //OK
        return *p + *q; // UB
    }

    while((p = get_x().arr, *p)) { // OK
        return *p; // UB
    }
    while((p = get_x().arr, *p) && f()){ //OK
        return *p; //UB
    }
    int d = (p = get_x().arr, *p) && *p; // OK

    return y;
}
