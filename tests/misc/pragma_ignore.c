/* run.config
   OPT: -print
*/

// Test that some pragmas are ignored, instead of resulting in a syntax error

void omp(int a[64], int d) {
  // Based on llama2.c
  int i;
  #pragma omp parallel for private(i)
  for (i = 0; i < d; i++) {
  }
}

int main(void) {
  int a[42];
  omp(a, 42);
  return 0;
}
