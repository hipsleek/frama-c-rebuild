/* run.config*
STDOPT: #"-lib-entry"
*/

const char const_global[] = "const_global";
char global[] = "global";

char f(char* s,unsigned i) { return s[i]; }

int main() {
  char local[] = "local";
  char test = f("arg", 0);
  for (int i = 0; i < sizeof(local); i++) {
    test = f(local,i);
  }
  Frama_C_show_each(const_global[0], global[1], local[2], test);
}
