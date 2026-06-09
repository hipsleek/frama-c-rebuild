/* run.config
STDOPT: #"-pp-annot"
*/




struct dma {
  volatile unsigned int flag;
  volatile unsigned int mode;
};

#define BASE           ((char *)0x40000000)
#define PAGE           (BASE + (unsigned int)0x10000000)
#define DMA1           ((struct dma *)PAGE)
#define DMA2           ((struct dma *)(PAGE + (unsigned int)0x2))

extern unsigned int Rd_dma_flag(volatile unsigned int *p) ;

//@ volatile ((struct dma *)BASE)->flag reads Rd_dma_flag;

unsigned int main(void) {
  return DMA1->flag + DMA2->flag ;
}
