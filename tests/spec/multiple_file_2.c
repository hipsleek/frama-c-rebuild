/* run.config
   DONTRUN: linked with multiple_file_1.c which is the real test.
*/
/*@ requires y <= 0; */
int g(int y);


struct i_s1 {
    int i1;
    float i2;
};

struct typ1 {
    int tab[sizeof(struct i_s1)];
};

int init_typ1(struct typ1* s);

struct i_s2 {
    int i1;
    float i2;
};

struct typ2 {
    int tab[sizeof(struct i_s2)];
};

int init_typ2(struct typ2* s);

struct typ2 h;

struct typ3 {
    int tab[sizeof(void*)];
};

int init_typ3(struct typ2* s);

struct typ3 l;

struct typ4 {
    int tab[sizeof(char*)];
};

int init_typ4(struct typ4*);

struct typ4 m;


struct typ5 {
    int tab[sizeof(signed char*)];
};

int init_typ5(struct typ5*);

struct typ5 n;

union typ6 {
    int tab[sizeof(long)];
};

int init_typ6(union typ6);

union typ6 o;

extern int t1[sizeof(long)];
extern int t2[sizeof(long)];
extern int t3[sizeof(long)];

int t = sizeof(h.tab);

int(*p)(int,int);

int v1_ok = sizeof(p);

int(*p1)(int);

int v1_ok_2 = sizeof(p1);

int(f1)(int,int);

int v2 = sizeof(f1);

int(f2)(int);

int v2_ok_2 = sizeof(f2);

enum EN { AB, AC, AD };

int v3 = sizeof(enum EN);
