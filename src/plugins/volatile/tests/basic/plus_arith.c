/* run.config
STDOPT: #"-pp-annot"
*/




typedef unsigned Unsigned;
struct dma {
  volatile unsigned flag;
  volatile Unsigned mode;
};

#define ZA       ((unsigned)0x40000000)
#define ZB       ((Unsigned)0x20000000)
#define VA0      ((struct dma *) (ZA))
#define VB0      ((struct dma *) (ZB))
#define VA1      ((struct dma *) (ZA + (unsigned)0x10000000))
#define VA2      ((struct dma *) (ZA + (Unsigned)0x20000000))

extern unsigned Rd_vb0_flag(volatile unsigned *p) ;
extern unsigned Rd_va0_flag(volatile unsigned *p) ;
extern unsigned Rd_va1_flag(volatile unsigned *p) ;
extern Unsigned Rd_va2_flag(volatile Unsigned *p) ;

extern unsigned Rd_vb0_mode(volatile unsigned *p) ;
extern Unsigned Rd_va0_mode(volatile Unsigned *p) ;
extern unsigned Rd_va1_mode(volatile unsigned *p) ;
extern Unsigned Rd_va2_mode(volatile Unsigned *p) ;

//@ volatile VB0->flag reads Rd_vb0_flag;
//@ volatile VB0->mode reads Rd_vb0_mode;
//@ volatile VA0->flag reads Rd_va0_flag;
//@ volatile VA0->mode reads Rd_va0_mode;
//@ volatile VA1->flag reads Rd_va1_flag;
//@ volatile VA1->mode reads Rd_va1_mode;
//@ volatile VA2->flag reads Rd_va2_flag;
//@ volatile VA2->mode reads Rd_va2_mode;

unsigned main_flag(void) {
  return VB0->flag + VA0->flag + VA1->flag + VA2->flag;
}
unsigned main_mode(void) {
  return VB0->mode + VA0->mode + VA1->mode + VA2->mode;
}
