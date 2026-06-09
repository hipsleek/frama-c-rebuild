/* run.config
PLUGIN: @EVA_PLUGINS@
STDOPT:
*/

int x;
void main(int c) {
if (c) {
  unsigned char buf[sizeof(unsigned char[1]) + sizeof(x)];
  buf[sizeof(buf)];
}
else {
  x = 4;
  unsigned char buf[sizeof(unsigned char[1]) + x];
  buf[x];
}
}
