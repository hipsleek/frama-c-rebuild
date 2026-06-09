/* run.config_qualif
   DONTRUN:
*/

//@ predicate Obs(integer x);

struct S {
  int f;
  int g[4];
  int m[3][5];
};

/*@
  ensures F: Obs( \result.f );
  ensures G: Obs( \result.g[k] );
  ensures H: Obs( \result.m[i][j] );
 */
struct S load(struct S *x, int k, int i, int j)
{
  return *x;
}
