/* run.config
   STDOPT: +"-impact-annot main"
   */


int find(int x) { return x; }

int main()
{
  int a = find(1);
  /*@ impact_stmt; */
  int b = find(2);
  int c = find(b);
  int d = find(3);
  return c ;
}
