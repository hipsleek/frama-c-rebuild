// leads to a loop; taken from open-source-case-studies/cerberus

int main()
{
  int* a;
  a = (int*)(& a);
  return *a;
}
