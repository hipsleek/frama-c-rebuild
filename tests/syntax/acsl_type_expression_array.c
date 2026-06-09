#include <stddef.h>
typedef struct sub_type {
    int tag;
} sub_type;

typedef struct {
    int *ptr;
    int foo[sizeof(sub_type)];
} TEST_TYPE;

int main(){
  size_t test = sizeof(int[sizeof(sub_type)]);
  /*@ assert test == sizeof(int[sizeof(sub_type)]); */
}
