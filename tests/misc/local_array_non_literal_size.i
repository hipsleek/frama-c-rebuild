/* run.config
   PLUGIN: @EVA_PLUGINS@
   OPT: -eva -machdep gcc_x86_64
*/

void one_dim() {
  int empty_array[3%1] = { };
  int normal_array[3+1] = { 0, 1, 2, 3 };
}

void two_dim_literal() {
  int empty_array[1][0] = { };
  int normal_array[1][4] = { 0, 1, 2, 3 };
}

void two_dim_non_literal () {
  int empty_array[1][3%1] = { };
  int normal_array[1][3+1] = { 0, 1, 2, 3 };
}

int main() {
  one_dim();
  two_dim_literal();
  two_dim_non_literal();
}
