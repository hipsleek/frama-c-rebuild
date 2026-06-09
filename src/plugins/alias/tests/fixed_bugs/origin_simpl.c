/* run.config
   COMMENT: simplified version of origin.c
*/

char t[];

void f() {
  void *tmp;
  tmp = *((int *)t);
}
