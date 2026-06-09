/* run.config
   COMMENT: Recursive logic definitions requiring casts to GMP
*/

/*@ logic integer identity(integer n) = n <= 0 ? n : identity(n-1) + 1; */

int main(void) {
  int i = 1;
  /*@ assert identity(i) == i; */
}
