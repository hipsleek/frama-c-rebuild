// continue
//  {a, b} are aliased

int main ()
{
  int *a=0, *b=0, *c=0;
  for (int i=0; i<10 ;i++) {
    if (1) {
      a = b;
      continue;
    }
    a = c;
  }

  return 0;
}
