/*run.config*
  STDOPT: -aorai-automata %{dep:@PTEST_DIR@/@PTEST_NAME@.ya} -aorai-acceptance
*/
void init(int *a, int n)
{
  for(int i = 0; i < n; i++)
    *(a+i) = i;
}

int find(int *a, int n, int k)
{
  for(int i = 0; i < n; i++)
    if (a[i] == k) return i;
  return -1;
}

int main(void)
{
  int a[10];
  init(a, 10);
  if (find(a, 10, 5))
    return find(a, 10, 11);
}
