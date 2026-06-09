
//@ check lemma direct_in: 2 \in {1,2,3};
//@ check lemma direct_in_singleton: 2 \in {2};

//@ logic set<integer> Set1 = {1,2,3};
//@ check lemma indirect_in_constants: 2 \in Set1;
//@ check lemma indirect_not_in_constants: ! (4 \in Set1);
//@ check lemma indirect_equal_constants: Set1 == {1,2,3};
//@ check lemma indirect_not_equal_constants: Set1 != {0,1,2};

//@ logic integer i0 = 0;
//@ logic integer i1 = 1;
//@ logic integer i2 = 2;
//@ logic set<integer> Set3 = {i1,i2};
//@ check lemma indirect_in_logical: i2 \in Set3;
//@ check lemma indirect_not_in_logical: ! (i0 \in Set3);
//@ check lemma indirect_equal_logical: Set3 == {i1,i2};
//@ check lemma indirect_not_equal_logical: Set3 != {i0,i1};

//@ ghost int int1 = 1;
//@ ghost int int2 = 2;
//@ logic set<int> Set2 = {int1,int2};
//@ check lemma indirect_in_ghost: int2 \in Set2;
//@ check lemma indirect_equal_ghost: Set2 == {int1,int2};

/*@ check lemma test_union:
  @   \forall set<int> a,b,c;
  @   (a == b || b == c) ==>
  @   (\union(a,\union(b,c)) == \union(a,c));
  @*/

/*@ check lemma test_inter:
  @   \forall set<int> a,b,c;
  @   (a == b || b == c) ==>
  @   (\inter(a,\inter(b,c)) == \inter(a,c));
  @*/

/*@
  check lemma quantify: (\forall set<integer> s; 0 \in s) ==> \false;
*/
