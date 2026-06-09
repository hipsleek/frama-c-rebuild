#include <string.h>

void test_strcmp(void)
{
  int res = strcmp("hello", "world");
  //@ check res == 0;
}

void test_strcat(void)
{
  char string[10];
  string[0] = 0;
  strcat(string, "hello");
  char string2[10];
  string2[0] = 0;
  strcat(string2, string);
}

volatile int nondet;
void test_strstr(void)
{
  char *s = nondet ? "aba" : "bab";
  char *needle = nondet ? "a" : "b";
  char *res = strstr(s, needle);
  //@ check res != 0;
}

void test_strncat(void)
{
  char data[100];
  data[0] = '\0';
  char source[100];
  //@ \eva::slevel 99;
  for (int i = 0; i < 99; i++) source[i] = 'Z';
  source[99] = '\0';
  strncat(data, source, 100);

  //     index:    0    1   2   3    4   5   6    7
  char buf1[] = { 'a', 'b', 0, 'c', 'd', 0, 'f', 'g' };
  strncat(buf1+3, buf1, 2); // valid: 'ab\0cdab\0'

  if (nondet) {
    char buf1[] = { 'a', 'b', 0, 'c', 'd', 0, 'f' };
    strncat(buf1+3, buf1, 2); // invalid: strncat will add a final '\0'
    //@ assert unreachable: \false;
  }
}

struct s {
  char s1[30];
  char s2[30];
};

// this test crashes GCC (tested with v7.1.1) due to the non-respect of
// non-aliasing in strcpy
void crashes_gcc() {
  struct s s;
  char *ss = "ABCDEFGHIJKLMNOPQRSTUVWXYZ012";
  //@ \eva::slevel 30;
  for (int i = 0; i < 30; i++) s.s1[i] = ss[i];
  char *dest = s.s1+29;
  char *src = s.s1;
  strcpy(dest, src); // must produce at least a warning
}

void test_strtok() {
  if (nondet) {
    strtok(NULL, " "); // must fail
    //@ assert unreachable: \false;
  }
  char buf[2] = {0};
  char *a = strtok(buf, " ");
  //@ check a == \null || \subset(a, buf+(0..));
  char *b = strtok(NULL, " ");
  //@ check b == \null || \subset(b, buf+(0..));
  char buf2[4] = "abc";
  char *p = strtok(buf2, "b");
  //@ check p == \null || \subset(p, buf2+(0..));
  char *q = strtok(NULL, "c");
  //@ check q == \null || \subset(p, buf2+(0..));
  // test with non-writable string, but delimiter not found
  char *r = strtok((char*)"constant!", "NONE_TO_BE_FOUND");
  //@ check r == \null;
  if (nondet) {
    strtok((char*)"constant!", "!");
    //@ assert unreachable_if_precise: \false;
  }
}

void test_strtok_r() {
  if (nondet) {
    strtok_r(NULL, " ", NULL); // must fail
    //@ assert unreachable: \false;
  }
  char *saveptr;
  char buf[2] = {0};
  char *a = strtok_r(buf, " ", &saveptr);
  if (nondet) {
    strtok_r(buf, " ", NULL); // must fail
    //@ assert unreachable: \false;
  }
  //@ check a == \null || \subset(a, buf+(0..));
  char *b = strtok_r(NULL, " ", &saveptr);
  Frama_C_show_each_saveptr(saveptr);
  //@ check b == \null || \subset(b, buf+(0..));
  char buf2[4] = "abc";
  char *p = strtok_r(buf2, "b", &saveptr);
  //@ check p == \null || \subset(p, buf2+(0..));
  char *q = strtok_r(NULL, "c", &saveptr);
  //@ check q == \null || \subset(p, buf2+(0..));
  // test with non-writable string, but delimiter not found
  char *r = strtok_r((char*)"constant!", "NONE_TO_BE_FOUND", &saveptr);
  //@ check r == \null;
  if (nondet) {
    strtok_r((char*)"constant!", "!", &saveptr);
    //@ assert unreachable_if_precise: \false;
  }
}

void test_strncpy() {
  char src[] = { 'a', 'b', 'c' };
  char dst[3];
  strncpy(dst,src,3);
  char src2[3];
  src2[0] = 'a';
  src2[1] = 'b';
  if (nondet) {
    strncpy(dst,src2,3);
    //@ assert unreachable: \false;
  }
}

void test_strlcpy() {
  char buf[16];
  char buf2[32];
  size_t r1 = strlcpy(buf, "longer than buffer", 16);
  size_t r2 = strlcpy(buf2, "short", 16);
  size_t r3 = strlcat(buf2, buf, 32);
  char src[] = { 'a', 'b', 'c' };
  char dst[3];
  if (nondet) {
    strlcpy(dst,src,3);
    //@ assert unreachable: \false;
  }
}

void test_strrchr() {
  const char *s1 = "not a palyndrome";
  const char *p = strrchr(s1, 'o');
  //@ check p == s1 + strlen(s1) - 2;
  p = strrchr(s1, 'Z');
  //@ check p == \null;
}

int main(int argc, char **argv)
{
  test_strcmp();
  test_strcat();
  test_strstr();
  test_strncat();
  if (!nondet) crashes_gcc();
  test_strtok();
  test_strtok_r();
  char *a = strdup("bla"); // unsound; specification currently unsupported
  char *b = strndup("bla", 2); // unsound; specification currently unsupported
  char *strsig = strsignal(1);
  //@ check valid_read_string(strsig);
  test_strncpy();
  test_strlcpy();
  test_strrchr();
  char *c = "haystack";
  char d = nondet ? 'y' : 'k';
  char *chr1 = strchr(c, d); //@ check *chr1 == d;
  char *nul1 = strchrnul(c, d);
  d = nondet ? 'a' : 'n';
  char *chr2 = strchr(c, d);
  char *nul2 = strchrnul(c, d);
  char pdest[10];
  char *pend = mempcpy(pdest, "gnu-only function", 9);
  //@ check imprecise: pend == pdest + 9 && *pend == '\0';

  char *rchr = memrchr(c, 'a', strlen(c));
  //@ check imprecise: rchr == c + strlen("haysta");
  rchr = memrchr(c, 'n', strlen(c));
  //@ check imprecise: rchr == \null;

  char mm_haystack[] = { 'I', 'h', 'a', 'v', 'e', '\0', 'z', 'e', 'r', 'o' };
  char mm_needle[] = { 'z', 'e', 'r', 'o' };
  char *memm =
    memmem(mm_haystack, sizeof(mm_haystack), mm_needle, sizeof(mm_needle));
  //@ check imprecise: memm == mm_haystack + 6;
  char mm_needle2[] = { '0' };
  memm =
    memmem(mm_haystack, sizeof(mm_haystack), mm_needle2, sizeof(mm_needle2));
  //@ check imprecise: memm == \null;

  char strsep_buf[8] = "a,b,,cc", *strsep_p1 = &strsep_buf[0];
  char strsep_needle[] = ",./";
  char *strsep_p2 = strsep(&strsep_p1, strsep_needle);
  //@ check \base_addr(strsep_p1) == \base_addr(strsep_p2);
  strsep_p2 = strsep(&strsep_p1, strsep_needle);
  //@ check \base_addr(strsep_p1) == \base_addr(strsep_p2);
  char *strsep_null = NULL;
  strsep_p2 = strsep(&strsep_null, strsep_needle);
  //@ check strsep_p2 == \null;

  char *stpncpy_src = "12345678";
  char stpncpy_dest[5];
  char *stpncpy_res = stpncpy(stpncpy_dest, stpncpy_src, 5);
  //@ check stpncpy_res == stpncpy_dest + 5;
  //@ check \forall integer i; 0 <= i < 5 ==> stpncpy_dest[i] != 0;
  char *stpncpy_src2 = "12";
  stpncpy_res = stpncpy(stpncpy_dest, stpncpy_src2, 5);
  //@ check stpncpy_dest[0] == '1' && stpncpy_dest[1] == '1';
  //@ check stpncpy_dest[2..4] == 0;
  //@ check stpncpy_res == stpncpy_dest + 2;

  char *strlcat_src = "cat";
  char strlcat_buf[10] = "dog";
  size_t strlcat_res = strlcat(strlcat_buf, strlcat_src, 10); // "dogcat"
  //@ check imprecise: strlcat_res == 6;
  strlcat_res = strlcat(strlcat_buf, strlcat_buf, 0); // no overlap: n too small
  char strlcat_buf2[10] = "dogcat";
  strlcat_buf2[3] = 0; // "dog\0at"
  if (nondet) {
    // must fail: overlap between 'future string' "dogat" and source "at"
    strlcat_res = strlcat(strlcat_buf2, strlcat_buf2 + 4, 10); // overlap
    //@ assert unreachable: \false;
  }
  // no overlap: n too small; no changes to buffer
  strlcat_res = strlcat(strlcat_buf2, strlcat_buf2 + 4, 4);
  char strlcat_buf3[10] = "dogcat";
  strlcat_buf3[3] = 0; // "dog\0at"
  // no overlap: "dogt\0" and "t"
  strlcat_res = strlcat(strlcat_buf3, strlcat_buf3 + 5, 10);
  // check imprecise: strlcat_res == 5;

  char *strxfrm_src = "harr";
  // dest can be NULL if length is zero
  size_t strxfrm_res = strxfrm(0, strxfrm_src, 0);
  char strxfrm_dest[10];
  strxfrm_res = strxfrm(strxfrm_dest, strxfrm_src, 10);

  return 0;
}
