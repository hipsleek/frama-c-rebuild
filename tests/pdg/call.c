/* run.config
   LOG: call.f.dot call.g.dot
   STDOPT: +"-lib-entry -main g -pdg -pdg-dot ./call "
*/

/* Ne pas modifier : exemple utilisé dans le manuel du PDG. */

/*BDOC*/
struct {int a; int b; } G;
int A, B;

int f (int a, int b) {
    G.b = b;
    return a + G.a;
}

int g (int x, int y, int z) {
  int r =  f (x+y, z);
  A = G.a;
  B = G.b;
  return r;
}
