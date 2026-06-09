/* run.config
   STDOPT: +"%{dep:./enum_merge2.i} %{dep:./enum_merge3.i}"
*/

enum E {
 A = 1, B = 2, C = A+2
};

enum F {
 D = -5, E = D + 4
};

extern enum E x;

extern enum F t;

enum E y = C;

enum F u = E;
