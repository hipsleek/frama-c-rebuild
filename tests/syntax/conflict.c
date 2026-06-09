const int a[1] = { 0 };
int b = a[0];

struct stru {
    const int c;
};

const struct stru d = { 0 };

int e = d.c;

struct stru2 {
    int f;
};

const struct stru2 g = { 0 };

int h = g.f;
