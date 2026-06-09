/* run.config*
   ENV: FC_DETERMINISTIC yes
   STDOPT: +"-no-warn-unaligned-pointer -eva-domains equality,octagon -eva-octagon-through-calls -eva-msg-key=d-octagon,-d-cvalue -eva-oracle-depth 10"
*/

#include <stdint.h>

typedef struct cmd {
  uint16_t cmdLen;
  uint8_t cmdCode;
} cmd_t;

unsigned int const size = 436;
unsigned char buffer[436];

volatile char nondet;

void init () {
  for (int i = 0; i < size; i++) {
    buffer[i] = nondet;
  }
}

void cmdRead (unsigned char const *cmd) {
  uint16_t *len = (uint16_t *) cmd;
  uint8_t *code = (uint8_t *) (len + 1);
  uint32_t *elt1 = (uint32_t *) (code + 1); // elt1 = cmd + 3
  uint16_t *elt2 = (uint16_t *) (elt1 + 1);
  uint16_t *elt3 = (uint16_t *) (elt2 + 1);
  if (*len < sizeof(*code) + sizeof(*elt1) + sizeof(*elt2) + sizeof(*elt3)) // cmd->cmdLen >= m
    return;
  Frama_C_dump_each();
  uint8_t c = *code;
  uint32_t e1 = *elt1;
  uint16_t e2 = *elt2;
  uint16_t e3 = *elt3;
}

void main () {
  init ();
  unsigned int index = 3;
  cmd_t *cmd = (cmd_t *) &buffer[index];
  /*@ loop unroll 1; */
  while ( index + sizeof(cmd->cmdLen) + sizeof(cmd->cmdCode) <= size &&
          cmd->cmdLen != 0 &&
          size - index - sizeof(cmd->cmdLen) >= cmd->cmdLen) { // index <= n - cmd->cmdLen
    Frama_C_dump_each();
    cmdRead(&buffer[index]); // cmd = buffer + index
    index += cmd->cmdLen + sizeof(cmd->cmdLen);
    cmd = (cmd_t *) &buffer[index];
  }
}

#include "__fc_builtin.h"

void main2(void) {
    int t[10];
    int i = Frama_C_interval(0,9);
    int* p = t + i;

    while (p < &t[10]) {
        Frama_C_dump_each1();
        *p = 0;
        Frama_C_dump_each2();
        p++;
        Frama_C_dump_each3();
    }
}

void main3 () {
  int index = 3;
  int x = Frama_C_interval(0,100);
  /*@ loop unroll 1; */
  while (500 - index >= x) {
    Frama_C_dump_each();
    index += x + 4;
    Frama_C_dump_each();
    x = Frama_C_interval(0,100);
    Frama_C_dump_each();
  }
}
