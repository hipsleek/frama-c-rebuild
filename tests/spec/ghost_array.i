/* run.config
LOG: @PTEST_NAME@_pretty.c
OPT: -print -ocode @PTEST_NAME@_pretty.c -then @PTEST_NAME@_pretty.c -print -ocode=""
*/

typedef unsigned char uint8;
typedef unsigned short uint16;
typedef unsigned long uint32;

extern const uint32 t[2];
uint32 s[2];

/*@ predicate equal_t_s(integer i)=
  t[i] == (uint16)(*((uint8*)s+i) == * ((uint8*)s+i+1)); */

uint8 Array[10];
/*@ ghost uint32 FCG_Number; */
/*@ ghost uint32 FCG_Var1[1000]; */
/*@ ghost uint32 FCG_Var2[1000]; */

/*@
predicate mypredl{L}=
  (\forall integer MyNum; 
     0 <= MyNum < FCG_Number ==> FCG_Var2[MyNum] ==
     ((uint16)(*((uint8 *)(&Array[FCG_Var1[MyNum]] + (1))) * 256 + *((uint8 *)((&Array[FCG_Var1[MyNum]] + (1 + 2)) + 1)))) ) ;
*/
