/* run.config*
  STDOPT: #"-kernel-warn-key parser:decimal-float=warning"
  STDOPT: #"-kernel-warn-key parser:decimal-float=warning -eva-all-rounding-modes-constants -float-print hex"
*/

double f1 = 1e-40f;
double d0 = 1e-40;

int main()
{
  Frama_C_dump_each();
  double d1 = f1;
}
