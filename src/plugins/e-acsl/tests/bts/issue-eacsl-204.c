/*
  run.config
  COMMENT: issue eacsl#204: name conflict when a function
  COMMENT: with contract shares a name with a logic function
*/

/*@ predicate equalTag(integer n) = n; */

/*@ ensures equalTag(1); */
int equalTag() {
  return 1;
}

int main(void) {
  return 0;
}
