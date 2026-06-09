// left-hand side is set, right-hand side plain arithmetic type
/*@ lemma test1: { 0, 1 } + 1 == { 1, 2 }; */
/*@ lemma test2: 0 * { 0, 1 } == 0; */
/*@ lemma test3: {0,1} / 2 == 0; */

//right-hand side is set, left-hand side plain arithmetic type
/*@ lemma test4: 1 + { 0, 1 } == { 1, 2 }; */
/*@ lemma test5: { 0, 1 } * 0 == 0; */
/*@ lemma test6: 0 / {1,2} == 0; */

// both operands are sets.

/*@ lemma test7: {0,1} + {0,1} == {0,1,2}; */
/*@ lemma test8: {0, 1 } * {0,1} == {0,1}; */
/*@ lemma test9: {0,1} / {2,3} == 0; */

// original bug report (ill-typed on purpose, should warn, not crash)

/*@
  requires \valid(a + (0..0));
*/
void foo(int a);
