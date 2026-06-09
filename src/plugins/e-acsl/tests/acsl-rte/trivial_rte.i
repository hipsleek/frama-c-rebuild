/* run.config
 * COMMENT: Check that the RTE guards are generated at the right place.
 * STDOPT: #"-e-acsl-O 0"
*/

int main(void) {

  /*@ assert 4 / 2 == 2; */  // trivial case for division by zero

  return 0;
}
