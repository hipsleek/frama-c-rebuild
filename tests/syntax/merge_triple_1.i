/* run.config
   EXIT : 0
   OPT: %{dep:./merge_triple_2.i} %{dep:./merge_triple_3.i} -print
   OPT: %{dep:./merge_triple_3.i} %{dep:./merge_triple_2.i} -print
*/
typedef unsigned char BYTE;
typedef unsigned int WORD;

typedef struct _s1 {
  BYTE v;
} s1;

typedef struct _s2 {
  unsigned char i;
  WORD l;
  BYTE t[1];
} s2;

typedef union _u {
  s2 debug;
} u;

typedef struct _s4 {
  BYTE a;
} s4;

extern s4 S4[1];
