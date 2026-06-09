// Ignore clang warning emitted on pragma messages
#pragma clang diagnostic ignored "-W#pragma-messages"

#if defined(TEST_QUOTED_1) && defined(TEST_QUOTED_2) && TEST_QUOTED_2 == 2
  // ok
#else
  #warning "Unable to parse TEST_QUOTED_1 and TEST_QUOTED_2"
#endif

#if defined(TEST_UNQUOTED_1) && defined(TEST_UNQUOTED_2) && TEST_UNQUOTED_2 == 4
  // ok
#else
  #warning "Unable to parse TEST_UNQUOTED_1 and TEST_UNQUOTED2"
#endif

#if defined(TEST_QUOTED_STRING_1) && defined(TEST_QUOTED_STRING_2)
  #pragma message("Quoted test strings are '" TEST_QUOTED_STRING_1             \
                  "' and '" TEST_QUOTED_STRING_2 "'")
#else
  #warning "Unable to parse TEST_QUOTED_STRING"
#endif

#if defined(TEST_UNQUOTED_STRING_1) && defined(TEST_UNQUOTED_STRING_2)
  #define _STRINGIFY(str) #str
  #define STRINGIFY(str) _STRINGIFY(str)
  #pragma message("Unquoted test strings are '"                                \
                  STRINGIFY(TEST_UNQUOTED_STRING_1) "' and '"                  \
                  STRINGIFY(TEST_UNQUOTED_STRING_2) "'")
#else
  #warning "Unable to parse TEST_UNQUOTED_STRING"
#endif

struct t {
  int a;
};

int incr(int a) { return a + 1; }

int main(int argc, const char **argv) {
  if (argc > 1 && argc < 10) {
    return argc + 3;
  } else {
    return 0;
  }
}
