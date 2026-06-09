union U {
    short s[2];
    int i;
};

void f (union U *p) {
    p->i++;
    p->s[0]++;
    p->s[1]++;
}
