// conditional cfg
//  {a, b, c} are aliased

int main () {

  int *a=0, *b=0, *c=0;
  if (a) {
    a = b;
  }
  else {
    a = c;
  }
  *a = 4;  
  return 0;
}
