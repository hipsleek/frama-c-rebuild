/* run.config
  STDOPT:
*/

#include <endian.h>

int main() {
  int16_t i16 = 1234;
  int32_t i32 = 1234;
  int64_t i64 = 1234;

  uint16_t be16 = htobe16(i16);
  uint32_t be32 = htobe32(i32);
  uint64_t be64 = htobe64(i64);

  int16_t h16 = be16toh(be16);
  int32_t h32 = be32toh(be32);
  int64_t h64 = be64toh(be64);

  //@ assert h16 == 1234;
  //@ assert h32 == 1234;
  //@ assert h64 == 1234;

  uint16_t le16 = htole16(i16);
  uint32_t le32 = htole32(i32);
  uint64_t le64 = htole64(i64);

  h16 = le16toh(le16);
  h32 = le32toh(le32);
  h64 = le64toh(le64);

  //@ assert h16 == 1234;
  //@ assert h32 == 1234;
  //@ assert h64 == 1234;

  return 0;
}
