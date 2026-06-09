/* run.config
   STDOPT: #""
   EXIT: 3
   STDOPT: #"-cpp-extra-args=-DMULTIVLA1"
   STDOPT: #"-cpp-extra-args=-DMULTIVLA2"
   STDOPT: #"-cpp-extra-args=-DMULTIVLA3"
*/

const int n = 10;

void main() {
  int a[n][42]; // single variable length dimension
  a[0][0] = 1;
  int b[a[0][0]][5][10]; // idem

// arrays with non-first variable dimensions; not currently supported
#ifdef MULTIVLA1
  int c[n][n];
#endif
#ifdef MULTIVLA2
int d[42][n];
#endif
#ifdef MULTIVLA3
  int e[1][n][9];
#endif
}
