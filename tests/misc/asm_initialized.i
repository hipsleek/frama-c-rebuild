/* run.config*
PLUGIN: @EVA_PLUGINS@ report
STDOPT: #"-asm-contracts-ensure-init -asm-contracts-auto-validate -inline-stmt-contracts -absolute-valid-range 0x10000000-0xf00000000 -print -report"
*/

int main() {
  int* sp;
  int x = 2;
  asm volatile ("mov %%rsp, %0;":"=r"(sp));
  *(sp - 2) = 3;
  return x;
}
