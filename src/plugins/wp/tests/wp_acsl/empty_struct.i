/* run.config
OPT: -machdep gcc_x86_64
*/
/* run.config_qualif
OPT: -machdep  gcc_x86_64
*/

struct S { };

struct S a1[1];
struct S a2[0xFFFFFFFFF];

/*@ assigns a1[0..] \from a1[0..]; */
extern void f1(void);

/*@ ensures a1[0] == \at(a1[0], Pre);
  @ assigns a1[0..] \from a1[0..];
*/
void g1() {
  f1();
}

/*@ assigns a2[0..] \from a2[0..]; */
extern void f2(void);

/*@ ensures a2[0] == \at(a2[0], Pre);
  @ assigns a2[0..] \from a2[0..];
*/
void g2() {
  f2();
}

int main(void) {
  g1();
  g2();
}
