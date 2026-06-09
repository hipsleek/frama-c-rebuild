inductive gcd(integer n, integer m, integer r) {
    case gcd_zero: \forall integer x; gcd(x, 0, x);
    case gcd_S: \forall integer x, y, z;
        y != 0 ==> gcd(y, x % y, z) ==> gcd(x, y, z);
}
