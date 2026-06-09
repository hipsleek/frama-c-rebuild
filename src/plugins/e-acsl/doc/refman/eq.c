inductive eq(integer x, integer y) {
  case c: \forall integer a, b, c; a == c ==> b == c ==> eq(a, b);
}
