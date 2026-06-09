/* run.config*
  EXIT: 1
  STDOPT: #"-kernel-warn-key parser:decimal-float=warning"
  EXIT: 0
  STDOPT: #"-kernel-warn-key parser:decimal-float=warning -eva-all-rounding-modes-constants"
*/

double f1 = 3.4e38f;
double f2 = 3.405e38f;

int main()
{
  Frama_C_dump_each();
  double d2 = f2;
}
