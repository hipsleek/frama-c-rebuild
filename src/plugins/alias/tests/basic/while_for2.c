// while loops with trivial conditions
//  {a, b} are aliased

int main ()
{
  int *a=0, *b=0, *c=0;
  while (1) {
    a = b;
    break;
  }

   while (0) {
    a = c;
    break;
  }
  return 0;
}
