//@ predicate EQ(real a, double b) = \abs(a-b) < \max(a,b) * 1e-16 ;

/*@ assigns \nothing; */
void add(double x, double y)
{
  /*@ check A:  EQ(    1234567,
                       1234567.0d ); */
  /*@ check B:  EQ(  12.345670,
                     12.345670d   ); */
  /*@ check C:  EQ(  12.345670e4,
                     12.345670e4d ); */
  /*@ check C:  EQ(  12.345670e-4,
                     12.345670e-4d ); */
  /*@ check D:  EQ( .1234567e4,
                    .1234567e4d ); */
  /*@ check E:  EQ( .1234567e-4,
                    .1234567e-4d ); */
  return ;
}
