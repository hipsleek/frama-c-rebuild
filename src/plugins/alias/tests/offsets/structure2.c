// structure with initialisation and pointer
//  no alias

typedef struct
{       
    int   a;
    int   b;
} st_1_t;

typedef struct
{       
    struct struct_1_t*  s;
    int   c;
} st_2_t;


int main () {

  st_1_t x1 = {0,1};
  st_1_t x2 = {1,2};
  st_2_t y = {&x1,4};

  y.s = &x2;

  return 0;
}
