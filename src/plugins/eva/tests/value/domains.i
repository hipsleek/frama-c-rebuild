/* run.config
   LOG: domains_dump_0
   STDOPT: #"-eva-domains sign,equality,bitwise,symbolic-locations,gauges,octagon,multidim -eva-msg-key=d-equality,d-symbolic-locations,d-gauges,d-octagon,d-multidim,d-bitwise,d-sign -eva-warn-key experimental=inactive"
*/
/* run.config*
   DONTRUN: avoids duplication of domains_dump oracle
*/

volatile int nondet;

/*  Tests multiple domains together. */
void main (int a) {
  int i, j, k, r;
  int t[100];
  /* Tests the multidim domain: the array is initialized after the loop. */
  for (i = 0; i < 100; i++) {
    t[i] = nondet;
  }
  i = t[64];
  /* Tests the equality domain: i is reduced through the condition,
     no invalid read. */
  int b1 = i <= 0;
  int b2 = i >= 100;
  if (b1 || b2)
    i = 1;
  r = t[i];
  /* Tests the symbolic locations domain: t[i]/i is smaller than 1000,
     no overflow. */
  if (t[i] / i < 1000)
    r = t[i] / i + 1;
  /* Tests the gauges domain: k==i during the loop, no overflow. */
  k = 0;
  j = 10;
  while (k < 200 && nondet) {
    k++;
    j++;
  }
  /* Tests the octagon domain. */
  r = i - j + 1;
  k = r + j;
  r = t[k-1];
  /* Tests the sign domain: no division by zero. */
  if (a != 0)
    r = 100 / a;
  /* Tests the bitwise domain: a == 8, no division by zero. */
  a = (a | 8) & 8;
  r = 10 / a;

  /* Tests printing of domains. */
  Frama_C_domain_show_each(a, i, j, k, r);
  Frama_C_dump_each();
  Frama_C_dump_each_file_domains_dump();

  /* Tests printing offsetmap of scalar value. */
  int scalar = 65537;
  *(((char *)&scalar)+1) = 42;
  Frama_C_domain_show_each(scalar);
}
