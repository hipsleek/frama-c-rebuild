/* run.config
EXIT: 1
STDOPT:
*/

/*@ assigns *p \from \nothing; */
void f(char *p)

/*@
  assigns \result \from \nothing;
  ensures \result == 0;
*/
char g(char *p);
