/* Test of error messages when a builtin or a specification is used for the
   main function. */

/*@ assigns \nothing; */
void spec_only(int a);

/*@ assigns \result \from \nothing; */
long strlen(char *s);

//@ assigns \result \from \nothing;
int main(void) {
  return 0;
}
