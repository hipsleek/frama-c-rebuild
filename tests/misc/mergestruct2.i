/* run.config
   OPT: -print %{dep:./mergestruct3.i} %{dep:./mergestruct1.i}
*/
struct s *p;

void g(void)
{
  p = 0;
}
