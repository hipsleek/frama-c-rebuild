/* run.config

STDOPT: #"-constfold"



 */
struct dma { unsigned int reg; };
typedef volatile struct dma vdma;
#define BASE ((unsigned int)0x00000001)
#define PAGE (BASE + (unsigned int)0x00000002)
#define DMA  (PAGE + (unsigned int)0x00000004)

extern unsigned int DMA_Rd_dma_reg(volatile unsigned int *p);
extern struct dma   DMA_Rd_dma(vdma *p);

struct dma   DMA_Rd_dma_bis(vdma *p);
//@ volatile ((vdma *)7)->reg reads DMA_Rd_dma_reg;
//@ volatile *((vdma *)7)    reads DMA_Rd_dma;

struct dma x;
unsigned int y;
int main (void) {
  x = *((vdma *)7);
  y = ((vdma *)7)->reg;

  x = *((vdma *)DMA);
  y = ((vdma *)DMA)->reg;

  return 1;
}
