inductive fibo(integer i, integer x) {
   case zero: fibo(0, 0);
   case one: fibo(1, 1);
   case other: \forall integer n, f1, f2;
       n>1 ==> fibo(n-1, f1) ==>
       \let nm2 = n-2; fibo(nm2, f2) ==> fibo(n, f1+f2);
}
