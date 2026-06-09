/*run.config
 EXIT: 1
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align1.c}" +"%{dep:./merge_attrs_align2.c}"
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align1.c}" +"%{dep:./merge_attrs_align3.c}"
 EXIT: 0
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align1.c}" +"%{dep:./merge_attrs_align4.c}"
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align1.c}" +"%{dep:./merge_attrs_align5.c}"
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align2.c}" +"%{dep:./merge_attrs_align3.c}"
 EXIT: 1
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align2.c}" +"%{dep:./merge_attrs_align4.c}"
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align2.c}" +"%{dep:./merge_attrs_align5.c}"
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align3.c}" +"%{dep:./merge_attrs_align4.c}"
  STDOPT: +"-machdep x86_64" +"%{dep:./merge_attrs_align3.c}" +"%{dep:./merge_attrs_align5.c}"

  COMMENT: to test offset of field 'b' in the code compiled by gcc/clang, do e.g.
  COMMENT: gcc -o ./merge_attrs_align merge_attrs_align*.c && ./merge_attrs_align

 */

// for testing with GCC/Clang
#ifndef __FRAMAC__
typedef struct {
  char a;
  short b;
} s;

s s1;

extern int f1();
extern int f2();
extern int f3();
extern int f4();
extern int f5();

int main() {
  f1(); // 1
  f2(); // 2
  f3(); // 2
  f4(); // 1
  f5(); // 1
  return 0;
}
#endif
