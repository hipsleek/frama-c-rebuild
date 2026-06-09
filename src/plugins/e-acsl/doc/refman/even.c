inductive even(integer x) {
  case zero: \forall integer a; even(0);
  case pos: \forall integer a; a >= 2 ==> even(a-2) ==> even(a);
  case neg: \forall integer a; a <= -2 ==> even(a+2) ==> even(a);
}
