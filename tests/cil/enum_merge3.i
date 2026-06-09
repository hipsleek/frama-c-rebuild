/* run.config
   STDOPT: +"%{dep:./enum_merge1.i} %{dep:./enum_merge2.i}"
*/

enum E {
  A = 3, B = 4, C = 5
};

enum F {
  D = -1, E = 4
};

enum E x = C;

enum F t = D;
