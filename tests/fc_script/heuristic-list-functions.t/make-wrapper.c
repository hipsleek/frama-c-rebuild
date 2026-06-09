int defined(int a);

int specified(int a);

int external(int a);

int large_name_to_force_line_break_in_stack_msg(void) {
  return large_name_to_force_line_break_in_stack_msg();
}

int rec(void) {
  return large_name_to_force_line_break_in_stack_msg();
}

int main() {
  int a = 42;
  a = rec();
  a = defined(a);
  a = specified(a);
  a = external(a);
}
