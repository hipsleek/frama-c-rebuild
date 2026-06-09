/* run.config
   EXIT : 0
   OPT: %{dep:./merge_triple_2.i} %{dep:./merge_triple_1.i} -print
   OPT: %{dep:./merge_triple_1.i} %{dep:./merge_triple_2.i} -print
*/
typedef unsigned char BYTE;
typedef unsigned int WORD;

typedef struct _s1 {
  BYTE v  __attribute__((__aligned__(1)));
} __attribute__((__packed__)) s1 __attribute__((__aligned__(1)));

typedef struct _s2 {
  BYTE i  __attribute__((__aligned__(1)));
  WORD l  __attribute__((__aligned__(1)));
  BYTE t[1]  __attribute__((__aligned__(1)));
} __attribute__((__packed__)) s2 __attribute__((__aligned__(1)));

typedef union _u {
  s2 s2  __attribute__((__aligned__(1)));
} __attribute__((__packed__)) u __attribute__((__aligned__(1)));

typedef struct _s3 {
  s1 h  __attribute__((__aligned__(1)));
  u u  __attribute__((__aligned__(1)));
} __attribute__((__packed__)) s3 __attribute__((__aligned__(1)));

typedef struct _s4 {
  BYTE a  __attribute__((__aligned__(1)));
} __attribute__((__packed__)) s4 __attribute__((__aligned__(1)));

s4 S4[1];

extern s3 S;
void g() {
  S.u.s2.i = 1;
}
