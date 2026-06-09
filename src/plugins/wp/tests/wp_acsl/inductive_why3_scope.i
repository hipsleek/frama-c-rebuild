/* run.config
  OPT: -wp-gen -wp-prover why3 -wp-msg-key print-generated
*/

/*@ inductive i(integer x) {
      case zero: i(0);
      case ind: \forall integer n; i(n) ==> i(n+1);
    }

    inductive d(integer x) {
      case zero: d(0);
      case ind: \forall integer n; d(n) ==> d(n-1);
    }

    lemma l: i(10) && d(-10);
*/

/*@ axiomatic Ax {
      inductive i2(integer x) {
        case zero: i2(0);
        case ind: \forall integer n; i2(n) ==> i2(n+1);
      }

      inductive d2(integer x) {
        case zero: d2(0);
        case ind: \forall integer n; d2(n) ==> d2(n-1);
      }

      lemma l2: i2(10) && d2(-10);
    }
*/
