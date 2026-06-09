int f(int x);

int (*ptr)(int x) = f;

//@ predicate P(int(*pf)(int)) = pf == f;

//@ predicate P_KO(int(*pf)(int x)) = pf == f;
