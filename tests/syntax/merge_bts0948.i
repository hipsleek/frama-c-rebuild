/* run.config
   STDOPT: +"%{dep:./merge_bts0948_1.i}" +"%{dep:./merge_bts0948_2.i}"
*/

/*@ requires \valid((char*)dest);
*/
extern void *memcpy(void * dest);

void* memcpy(void* region1) { return region1; }
