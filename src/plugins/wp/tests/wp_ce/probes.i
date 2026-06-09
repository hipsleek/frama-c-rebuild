/* run.config_qualif
   DONTRUN:
*/
/* run.config_ce
   DONTRUN:
*/

// Matrix addressing

int a[3][4][5];

//@ ensures \false; assigns \nothing;
void get(int i, int j, int k)
{
 A:
  if (i) i++;
 B:
  if (j) i++;
 C:
  if (k) i++;
  //@ probe A: \at(i,A) - \at(i,Pre);
  //@ probe B: \at(i,B) - \at(i,Pre);
  //@ probe C: \at(i,C) - \at(i,Pre);
  //@ probe Offset: &a[i][j][k] - &a[0][0][0];
  return;
}
