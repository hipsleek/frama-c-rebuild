/* run.config*

   STDOPT: #"-absolute-valid-range 0-0x3"
   STDOPT: #"-main negative_absolute_address -absolute-valid-range 0-0x10000000000000000"
   STDOPT: #"-main negative_absolute_address -warn-invalid-pointer -absolute-valid-range 0-0x10000000000000000"
*/

int * f() {
  return 100;
}

void crash () {
  unsigned int v = 1;
  *((f()))=v;
}


char R;
void main(int c) {
  if(c) crash();

  *((char*)0)=2;
  R = *((char*)1);
  *((char*)2)=2;
  R = *((char*)3);
}

#include <stdint.h>

volatile char nondet;

/* Tests negative absolute addresses. Currently, Eva interprets absolute
   addresses as unsigned integers of the size of pointers, and wrap negative
   addresses accordingly. It would be better to not wrap such negative addresses
   and to warn when they are dereferenced.
   With -warn-invalid-pointers, such negative absolute addresses raise a
   proper alarm. However, the same computation in uintptr_t type are always
   valid. */
void negative_absolute_address(void) {
  uintptr_t uptr = 0;
  char *p = 0;
  if (nondet) {
    *(p - 100) = 25; // invalid pointer
    Frama_C_show_each(p - 100);
  } else {
    *((char *)(uptr - 100)) = 1; // valid
    Frama_C_show_each((char *)(uptr - 100));
  }
  if (nondet) {
    char *q = p - 99; // invalid pointer
    *q = 42;
    Frama_C_show_each(q);
  } else {
    char *q = (char *)(uptr - 99); // valid
    *q = 2;
    Frama_C_show_each(q);
  }
}
