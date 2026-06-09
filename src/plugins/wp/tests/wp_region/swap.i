/*@
  requires \valid(a);
  requires \valid(b);
  region A: *a, B: *b;
  ensures *a == \old(*b);
  ensures *b == \old(*a);
  assigns *a, *b;
  */
void swap_separated(int *a, int *b)
{
  int tmp = *a ;
  *a = *b;
  *b = tmp ;
}

/*@
  requires \valid(a);
  requires \valid(b);
  region R: *a, *b;
  ensures *a == \old(*b);
  ensures *b == \old(*a);
  assigns *a, *b;
  */
void swap_aliased(int *a, int *b)
{
  int tmp = *a ;
  *a = *b;
  *b = tmp ;
}
