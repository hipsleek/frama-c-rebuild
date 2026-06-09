// switch
//  {a, b, d} are aliased

int main ()
{
  int *a=0, *b=0, *c=0, *d=0, e=0;
  switch (e) {
  case 1:
    a=d;
    break;
  case 2:
    b=d;
    break;
  }
    
  return 0;
}
