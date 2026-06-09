const int a = 0;
const int b = 1;
int c = a + b;

int d = (int) ((int*)3+5);

const int e = 42;
int f = (int) ((int*)e);

union uty {
    int g;
    float h;
};

const int i = 1;
union uty j = {i};

int k;
int l = sizeof(k);

int m;
int n = sizeof((int) &m);

struct sty {
    int o;
};

const int o = 1;
struct sty p = { o };

int q;
unsigned long r = (unsigned long) &q;

long s = (long) &s;
