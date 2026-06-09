// used to lead to a loop in the graph - fixed
// no alias since there is an infinite loop

int main()
{
  while (1) {
    int l[1] = {0};
    int *n_0 = & l[1];
    n_0 = & l[1] + 0;
    int w = 0;
    if (w)
      l[0] = *(& l[1] + 0);
  }
  return 0;
}
