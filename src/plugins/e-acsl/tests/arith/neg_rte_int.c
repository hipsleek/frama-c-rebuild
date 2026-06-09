/* run.config
   STDOPT: #"-rte"
   COMMENT: Generates an RTE with a negative integer
   COMMENT: (The negative integer being the lower bound of signed chars)
   COMMENT: This regression test ensures that E-ACSL correctly handles negative integers.
*/
char c;
float f;
void main() {
  c = f + 0.5;
}
