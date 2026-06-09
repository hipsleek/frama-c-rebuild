/* run.config
   STDOPT: +"%{dep:./enum_merge1.i} %{dep:./enum_merge3.i}"
*/

enum E {
  A = 2, B = A+1, C = 4
};

enum F {
  D = -2, E = D + 3
};

extern enum E x;

extern enum F t;

enum E z = B;

enum F v = E;
