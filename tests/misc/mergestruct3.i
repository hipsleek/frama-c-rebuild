/* run.config
   OPT: -print %{dep:./mergestruct1.i} %{dep:./mergestruct2.i}
   OPT: -print %{dep:./mergestruct2.i} %{dep:./mergestruct1.i}
*/
struct s { float a; } s2;

void f(void)
{
  s2.a = 1.0;
}
