/* run.config
   DONTRUN:
 */

/* run.config_qualif
   OPT: -wp-rte -warn-unsigned-downcast -warn-signed-downcast
*/

#include <stdint.h>

void xor8u(uint8_t x, uint8_t y) { x ^= y; }
void or8u(uint8_t x, uint8_t y) { x |= y; }
void and8u(uint8_t x, uint8_t y) { x &= y; }
//@ requires y < 8;
void sr8u(uint8_t x, uint8_t y) { x >>= y; }

void xor16u(uint16_t x, uint16_t y) { x ^= y; }
void or16u(uint16_t x, uint16_t y) { x |= y; }
void and16u(uint16_t x, uint16_t y) { x &= y; }
//@ requires y < 16;
void sr16u(uint16_t x, uint16_t y) { x >>= y; }

void xor8s(int8_t x, int8_t y) { x ^= y; }
void or8s(int8_t x, int8_t y) { x |= y; }
void and8s(int8_t x, int8_t y) { x &= y; }
//@ requires 0 <= y < 8;
void sr8s(int8_t x, int8_t y) { x >>= y; }

void xor16s(int16_t x, int16_t y) { x ^= y; }
void or16s(int16_t x, int16_t y) { x |= y; }
void and16s(int16_t x, int16_t y) { x &= y; }
//@ requires 0 <= y < 16;
void sr16s(int16_t x, int16_t y) { x >>= y; }
