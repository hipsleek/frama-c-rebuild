/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "__fc_builtin.h"
#include "errno.h"
#include "float.h" // for DBL_MAX
#include "stdbool.h"
#include "stdio.h"
#include "stdlib.h"
#include "stdint.h" // for SIZE_MAX
#include "sys/types.h" // for ssize_t
#include "__fc_file.c" // __fc_fopen initializers
__PUSH_FC_STDLIB

// Returns 1 iff mode contains a valid mode string for fopen() and
// related functions; that is, one of the following:
// "r","w","a","rb","wb","ab","r+","w+","a+",
// "rb+","r+b","wb+","w+b","ab+","a+b".
/*@
  requires valid_mode: valid_read_string(mode);
  assigns \result \from mode[0 .. strlen(mode)];
 */
static bool is_valid_mode(char const *mode) {
  if (!(mode[0] != 'r' || mode[0] != 'w' || mode[0] != 'a')) return false;
  // single-char mode string; ok
  if (!mode[1]) return true;
  // two- or three-char mode string
  if (!(mode[1] != 'b' || mode[1] != '+')) return false;
  // two-char mode string; ok
  if (!mode[2]) return true;
  if (mode[2] == mode[1] || !(mode[2] != 'b' || mode[2] != '+')) return false;
  // a three-char mode string is ok; everything else is not
  return !mode[3];
}

// inefficient but POSIX-conforming implementation of getline
ssize_t getline(char **lineptr, size_t *n, FILE *stream) {
  if (!lineptr || !n || !stream) {
    errno = EINVAL;
    //TODO: set error indicator for stream
    return -1;
  }
  if (ferror(stream) || feof(stream)) {
    //TODO: set error indicator for stream
    return -1;
  }
  if (!*lineptr || *n == 0) {
    *lineptr = malloc(2);
    if (!*lineptr) {
      errno = ENOMEM;
      //TODO: set error indicator for stream
      return -1;
    }
    *n = 2;
  }
  size_t cur = 0;
  while (!ferror(stream) && !feof(stream)) {
    while (cur < *n-1) {
      char c = fgetc(stream);
      if (c == EOF && cur == 0) {
        // no characters were read
        //TODO: set error indicator for stream
        return -1;
      }
      if (c != EOF) (*lineptr)[cur++] = c;
      if (c == '\n' || c == EOF) {
        // finished reading a line or the file
        (*lineptr)[cur] = '\0';
        return cur;
      }
    }
    // try to realloc larger buffer
    if (*n == SSIZE_MAX) {
      errno = EOVERFLOW;
      //TODO: set error indicator for stream
      return -1;
    }
    size_t new_size = *n+1;
    *lineptr = realloc(*lineptr, new_size);
    if (!*lineptr) {
      // failed to realloc larger line
      errno = ENOMEM;
      //TODO: set error indicator for stream
      return -1;
    }
    *n = new_size;
  }
  //TODO: set error indicator for stream
  return -1;
}

// Non-POSIX; arbitrarily allocates between 1 and 256 bytes.
// This stub is unsound in the general case, but enough for
// many test cases.
int vasprintf(char **strp, const char *fmt, va_list ap) {
  size_t len = Frama_C_interval(1, 256);
  *strp = malloc(len);
  if (!*strp) {
    return -1;
  }
  // Emulate writing to the string
  Frama_C_make_unknown(*strp, len - 1U);
  (*strp)[len - 1U] = 0;
  return len;
}

int asprintf(char **strp, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int res = vasprintf(strp, fmt, args);
  va_end(args);
  return res;
}

char *fgets(char *restrict s, int size, FILE *restrict stream) {
  if (Frama_C_interval(0, 1)) {
    // error
    int possible_errors[] = {
      EAGAIN,
      EBADF,
      EINTR,
      EIO,
      EOVERFLOW,
      ENOMEM,
      ENXIO,
    };
    errno = possible_errors[Frama_C_interval(0, sizeof(possible_errors)/sizeof(int)-1)];
    return 0;
  }
  int i = 0;
  for (; i < size-1; i++) {
    // Emulate reading a character from stream; either a "normal" character,
    // or EOF
    if (Frama_C_interval(0, 1)) {
      // Encountered an EOF: 0-terminate the string and return
      s[i] = 0;
      return s;
    }
    // Otherwise, encountered a "normal" character
    char c = Frama_C_interval(CHAR_MIN, CHAR_MAX);
    s[i] = c;
    if (c == '\n') {
      // in case of a newline, store it, then 0-terminate, then return
      s[i+1] = 0;
      return s;
    }
  }
  // 0-terminate the string after the last written character
  s[i] = 0;
  return s;
}

int fgetc(FILE *restrict stream) {
  if (Frama_C_interval(0, 1)) {
    // error
    int possible_errors[] = {
      EAGAIN,
      EBADF,
      EINTR,
      EIO,
      EOVERFLOW,
      ENOMEM,
      ENXIO,
    };
    errno = possible_errors[Frama_C_interval(0, sizeof(possible_errors)/sizeof(int)-1)];
    return EOF;
  }
  // From the POSIX manpage: "the fgetc() function shall obtain the next byte as
  //                          an unsigned char converted to an int (...) or EOF"
  if (Frama_C_interval(0, 1)) {
    return EOF;
  } else {
    return Frama_C_unsigned_char_interval(0, UCHAR_MAX);
  }
}

int getchar() {
  return fgetc(__fc_stdin);
}

// TODO: this stub does not ensure that, when fclose is called on the
// stream, the memory allocated here will be freed.
// (there is currently no metadata field in FILE for this information).
FILE *fmemopen(void *restrict buf, size_t size,
               const char *restrict mode) {
  if (!is_valid_mode(mode)) {
    errno = EINVAL;
    return NULL;
  }
  if (!buf) {
    if (size == 0) {
      // Some implementations may not support this; non-deterministically
      // return an error
      if (Frama_C_interval(0, 1)) {
        errno = EINVAL;
        return NULL;
      }
    }
    if (mode[1] != '+' && (mode[1] && mode[2] != '+')) {
      // null buffer requires an update ('+') mode
      errno = EINVAL;
      return NULL;
    }
    buf = malloc(size);
    if (!buf) {
      errno = ENOMEM;
      return NULL;
    }
  }
  // Code to emulate a possible exhaustion of open streams; there is currently
  // no metadata in the FILE structure to indicate when a stream is available.
  if (Frama_C_interval(0, 1)) {
    // emulate 'too many open streams'
    errno = EMFILE;
    return NULL;
  }
  return &__fc_fopen[Frama_C_interval(0, __FC_FOPEN_MAX-1)];
}

#include "stdarg.h"
#include "__fc_scanf_stub_helper.h"

// Internal function used by v*scanf; we never look at the actual "source"
// (string, FILE* or stdin)
int __fc_v_scanf(const char * restrict format, va_list arg) {
  const char *p = format;
  char conversion_counter = 0;
  while (*p) {
    if (*p == '%') {
      enum length_modifier lm = NONE;
      char asterisks = 0;
      p++;
      if (*p == '%') {
        break;
      }
      // skip any flags
      while (1) {
        switch (*p) {
        case '-':
        case '+':
        case ' ':
        case '#':
        case '0':
          break;
        default:
          goto post_flags;
        }
        p++;
      }
    post_flags:
      // skip field width
      while (*p >= '0' && *p <= '9') {
        p++;
      }
      // special field width
      if (*p == '*') {
        asterisks++;
        p++;
      }
      if (*p == '.') {
        // skip precision
        p++;
        while (*p >= '0' && *p <= '9') {
          p++;
        }
        // special precision
        if (*p == '*') {
          asterisks++;
          p++;
        }
      }
      // length modifier
      switch (*p) {
      case 'h':
        p++;
        if (*p == 'h') {
          p++;
          lm = HH;
        } else {
          lm = H;
        }
        break;
      case 'l':
        p++;
        if (*p == 'l') {
          p++;
          lm = LL;
        } else {
          lm = L;
        }
        break;
      case 'j':
        p++;
        lm = J;
        break;
      case 'z':
        p++;
        lm = Z;
        break;
      case 't':
        p++;
        lm = T;
        break;
      case 'L':
        p++;
        lm = UPPER_L;
        break;
      }
      // read asterisks
      while (asterisks) {
        // reading the arguments ensures that initialization errors are detected
        int ignored = va_arg(arg, int);
        (void)(ignored); // avoid GCC warning about unused variable
        asterisks--;
      }
      // conversion specifier
      switch (*p) {
      case 'd':
      case 'i':
        switch (lm) {
        case NONE:
          *va_arg(arg, int*) = Frama_C_interval(INT_MIN, INT_MAX);
          break;
        case HH:
          *va_arg(arg, char*) = Frama_C_char_interval(CHAR_MIN, CHAR_MAX);
          break;
        case H:
          *va_arg(arg, short*) = Frama_C_short_interval(SHRT_MIN, SHRT_MAX);
          break;
        case L:
          *va_arg(arg, long*) = Frama_C_long_interval(LONG_MIN, LONG_MAX);
          break;
        case LL:
        case UPPER_L: // 'Ld' is not in ISO C, but GCC/Clang treat it like 'lld'
          *va_arg(arg, long long*) =
            Frama_C_long_long_interval(LLONG_MIN, LLONG_MAX);
          break;
        case J:
          *va_arg(arg, intmax_t*) =
            Frama_C_intmax_t_interval(INTMAX_MIN, INTMAX_MAX);
          break;
        case Z:
          *va_arg(arg, size_t*) = Frama_C_size_t_interval(0, SIZE_MAX);
          break;
        case T:
          *va_arg(arg, ptrdiff_t*) =
            Frama_C_ptrdiff_t_interval(PTRDIFF_MIN, PTRDIFF_MAX);
          break;
        }
        break;
      case 'o':
      case 'u':
      case 'x':
      case 'X':
        switch (lm) {
        case NONE:
          *va_arg(arg, unsigned*) =
            Frama_C_unsigned_int_interval(0, UINT_MAX);
          break;
        case HH:
          *va_arg(arg, unsigned char*) =
            Frama_C_unsigned_char_interval(0, UCHAR_MAX);
          break;
        case H:
          *va_arg(arg, unsigned short*) =
            Frama_C_unsigned_short_interval(0, USHRT_MAX);
          break;
        case L:
          *va_arg(arg, unsigned long*) =
            Frama_C_unsigned_long_interval(0, ULONG_MAX);
          break;
        case LL:
        case UPPER_L: // 'Ld' is not in ISO C, but GCC/Clang treat it like 'lld'
          *va_arg(arg, unsigned long long*) =
            Frama_C_unsigned_long_long_interval(0, ULLONG_MAX);
          break;
        case J:
          *va_arg(arg, uintmax_t*) = Frama_C_uintmax_t_interval(0, UINTMAX_MAX);
          break;
        case Z:
          *va_arg(arg, size_t*) = Frama_C_size_t_interval(0, SIZE_MAX);
          break;
        case T:
          *va_arg(arg, ptrdiff_t*) =
            Frama_C_ptrdiff_t_interval(PTRDIFF_MIN, PTRDIFF_MAX);
          break;
        }
        break;
      case 'f':
      case 'F':
      case 'e':
      case 'E':
      case 'g':
      case 'G':
      case 'a':
      case 'A':
        switch (lm) {
        case NONE:
        case L:
          // no effect
          *va_arg(arg, double*) = Frama_C_double_interval(-DBL_MAX, DBL_MAX);
          break;
        case UPPER_L:
          // TODO: use Frama_C_long_double_interval when it will be supported
          {
            volatile long double vld = 0.0;
            *va_arg(arg, long double*) = vld;
          }
          break;
        default:
          // Undefined behavior
          //@ assert invalid_scanf_specifier: \false;
          ;
        }
        break;
      case 'c':
        switch (lm) {
        case NONE:
          *va_arg(arg, char*) = Frama_C_char_interval(CHAR_MIN, CHAR_MAX);
          break;
        case L:
          *va_arg(arg, wint_t*) = Frama_C_wint_t_interval(WINT_MIN, WINT_MAX);
        default:
          // Undefined behavior
          //@ assert invalid_scanf_specifier: \false;
          ;
        }
        break;
      case 's':
        switch (lm) {
        case NONE:
          // TODO: take into account field width
          Frama_C_make_unknown(va_arg(arg, char*),
                               Frama_C_size_t_interval(0, SIZE_MAX));
          break;
        case L:
          // TODO: take into account field width
          Frama_C_make_unknown_wchar(va_arg(arg, wchar_t*),
                                     Frama_C_size_t_interval(0, SIZE_MAX/sizeof(wchar_t)));
        default:
          // Undefined behavior
          //@ assert invalid_scanf_specifier: \false;
          ;
        }
        break;
      case 'n':
        switch (lm) {
        case NONE:
          *va_arg(arg, int*) = conversion_counter;
          break;
        case HH:
          *va_arg(arg, char*) = conversion_counter;
          break;
        case H:
          *va_arg(arg, short*) = conversion_counter;
          break;
        case L:
          *va_arg(arg, long*) = conversion_counter;
          break;
        case LL:
        case UPPER_L: // 'Ld' is not in ISO C, but GCC/Clang treat it like 'lld'
          *va_arg(arg, long long*) = conversion_counter;
          break;
        case J:
          *va_arg(arg, intmax_t*) = conversion_counter;
          break;
        case Z:
          *va_arg(arg, size_t*) = conversion_counter;
          break;
        case T:
          *va_arg(arg, ptrdiff_t*) = conversion_counter;
          break;
        }
        break;
        //TODO
      }
      conversion_counter++;
    }
    p++;
  }
  return conversion_counter;
}

int _scanf_possible_errors[] = {
  EAGAIN,
  EBADF,
  EILSEQ,
  EINTR,
  EINVAL,
  EIO,
  ENOMEM,
  ENXIO,
  EOVERFLOW,
};

#define N_SCANF_ERRORS (sizeof(_scanf_possible_errors)/sizeof(int))

int vfscanf(FILE * restrict stream, const char * restrict format, va_list arg) {
  if (Frama_C_interval(0, 1)) { // simulate an error
    errno = _scanf_possible_errors[Frama_C_interval(0, N_SCANF_ERRORS-1)];
    return EOF;
  }
  return __fc_v_scanf(format, arg);
}

int vscanf(const char * restrict format, va_list arg) {
  if (Frama_C_interval(0, 1)) { // simulate an error
    errno = _scanf_possible_errors[Frama_C_interval(0, N_SCANF_ERRORS-1)];
    return EOF;
  }
  return __fc_v_scanf(format, arg);
}

int vsscanf(const char * restrict str, const char * restrict format, va_list arg) {
  //@ check valid_read_string(str);
  if (Frama_C_interval(0, 1)) { // simulate an error
    errno = _scanf_possible_errors[Frama_C_interval(0, N_SCANF_ERRORS-1)];
    return EOF;
  }
  return __fc_v_scanf(format, arg);
}

char __fc_tmpnam[L_tmpnam];
#ifdef L_ctermid
char __fc_ctermid[L_ctermid];
#endif
char __fc_cuserid[9];

__POP_FC_STDLIB
