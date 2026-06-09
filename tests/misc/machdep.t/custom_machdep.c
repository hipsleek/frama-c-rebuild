// most of the following includes are not directly used, but they test if
// the custom machdep has defined the necessary constants
#include <ctype.h>
#include <inttypes.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <wchar.h>

int main() { return INT_MAX - CUSTOM_MACHDEP; }
