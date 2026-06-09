// address assignment
// {a, c} are aliased

int b=0, *a=&b, *c=&b;

int main () {
  return 0;
}
