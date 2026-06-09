typedef unsigned           uint32 ;
typedef unsigned char      uint8 ;

void assignment(uint8 * array){
  uint32 u32_0a = * (uint32 *) array ;
  array[7] = 0x00 ;
  uint32 u32_0b = * (uint32 *) array ;

  //@ check u32_0a == u32_0b ;
}

//@ assigns array[l] ;
void assigns(uint8* array, uint8 l);

//@ assigns array[b .. e] ;
void assigns_r(uint8* array, uint8 b, uint8 e);

//@ assigns { array[i] | integer i ; i == x || i == y } ;
void assigns_l2(uint8* array, uint8 x, uint8 y);

void assigns_clause(uint8 * array){
  uint32 u32_0a = * (uint32 *) array ;
  assigns (array, 7) ;
  uint32 u32_0b = * (uint32 *) array ;
  assigns_r (array, 4, 7) ;
  uint32 u32_0c = * (uint32 *) array ;
  //@ check u32_0a == u32_0b == u32_0c ;
  assigns_l2 (array, 4, 6) ;
  uint32 u32_0d = * (uint32 *) array ;
  //@ check u32_0c == u32_0d ;
}
