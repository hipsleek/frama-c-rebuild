/* run.config
  OPT: -tty
 */

int main(void) {
    float pi = 3.14; // Should issue a colored warning
    int t[100] = {0};
    int x = 1 + t[42];
    return x;
}
