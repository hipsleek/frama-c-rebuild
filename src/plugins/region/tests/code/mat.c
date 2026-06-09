#define N 3
#define M 4

struct Complex { double re, im; } X[N][M];

void KrobbersStyle(int i, int j, int k)
{ double p = X[i][j].re ; }

void BornatFieldStyle(int i, int j, int k)
{
  struct Complex *c = &X[i][j];
  double p = c->re;
}

void BornatArrayStyle(int i, int j, int k)
{
  struct Complex *c = &X[i][j];
  double p = c[k].re;
}

/*@
  region A: X[..][..].re;
  region A: X[..][..].im;
*/
void CompCertStyle(int i, int j, int k)
{ double *p = &X[i][j].re + k ; }
