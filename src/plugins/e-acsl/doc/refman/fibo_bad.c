inductive fibo(integer i, integer x) {
   case zero: \forall integer a; fibo(0, a+0-a);
   case one: \forall integer a; fibo(a+1-a, 1);
   case other: \forall integer n, f1, f2;
       n+f1>1+f1 ==> fibo(n-1, f1) ==> fibo(n-2, f2) ==> fibo(n, f1+f2);
}
