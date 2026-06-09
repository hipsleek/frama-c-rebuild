// simple structure
//  {p_x, p_y} are aliased


typedef struct
{       
    int   a;
    int   b;
} st_1_t;

typedef struct
{       
    int   a;
    int   c;
} st_2_t;


int main () {

  st_1_t x = {0,1};
  st_2_t y = {3,4};

  st_1_t * p_x = &x;
  st_2_t * p_y = &y;

  
  p_x->a = 3;
  p_x = p_y; 
  return 0;
}
