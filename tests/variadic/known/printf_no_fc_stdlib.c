/* run.config
   COMMENT: Only activate the test on linux x64 with glibc as it is dependent on
   COMMENT: the content of the system's stdio.h
   ENABLED_IF: (= %{ocaml-config:target} x86_64-pc-linux-gnu)
   STDOPT: #"-no-frama-c-stdlib" +"-kernel-msg-key=\"-variadic\" -kernel-warn-key=\"attrs=inactive\""
*/
#include <stdio.h>

int main() {
  printf("dummy call: %d\n", 1);
  printf("other call with more args: %d and %d\n", 2, 3);
}
