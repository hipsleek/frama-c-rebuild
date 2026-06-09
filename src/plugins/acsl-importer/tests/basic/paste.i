/* run.config
 DEPS: file1.acsl file2.acsl includes/file1.acsl includes/file2.acsl  includes/file3.acsl
   STDOPT: -acsl-import %{dep:./paste.acsl} -print -then -print -ocode ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then -no-print ./ocode_@PTEST_NUMBER@_@PTEST_NAME@.i
 */


int volatile z;


int rd_z_cpt;
extern int rd_z_tab[];
int rd_z(int volatile * p) {
  int result = rd_z_tab[rd_z_cpt];
  rd_z_cpt++ ;
  return result;
};

int main (int x) {
 l: { int y = x, x = y;
  l1: { int x = y ;
        if (x > 10)
          return x;
   l11: y = x;
      }
  l2: { int x = y + z;
   l21: return x + y;
      }
    }
}
