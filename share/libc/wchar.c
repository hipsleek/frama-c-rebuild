/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.25 */
#include "wchar.h"
#include "float.h" // for DBL_MAX
#include "__fc_file.c" // __fc_fopen initializers

__PUSH_FC_STDLIB

wchar_t* wmemcpy(wchar_t *dest, const wchar_t *src, size_t n)
{
  for (size_t i = 0; i < n; i++) {
    dest[i] = src[i];
  }
  return dest;
}

wchar_t * wmemset(wchar_t *dest, wchar_t val, size_t len)
{
  for (size_t i = 0; i < len; i++) {
    dest[i] = val;
  }
  return dest;
}

wchar_t* wcscpy(wchar_t *dest, const wchar_t *src)
{
  size_t i;
  for (i = 0; src[i] != L'\0'; i++)
    dest[i] = src[i];
  dest[i] = L'\0';
  return dest;
}

size_t wcslen(const wchar_t * str)
{
  size_t i;
  for (i = 0; str[i] != L'\0'; i++);
  return i;
}

wchar_t * wcsncpy(wchar_t *dest, const wchar_t *src, size_t n)
{
  size_t i;
  for (i = 0; i < n; i++) {
    dest[i] = src[i];
    if (src[i] == L'\0') break;
  }
  for (; i < n; i++)
    dest[i] = L'\0';
  return dest;
}

wchar_t * wcscat(wchar_t *dest, const wchar_t *src)
{
  size_t i;
  size_t n = wcslen(dest);
  for (i = 0; src[i] != L'\0'; i++) {
    dest[n+i] = src[i];
  }
  dest[n+i] = L'\0';
  return dest;
}

wchar_t* wcsncat(wchar_t *dest, const wchar_t *src, size_t n)
{
  size_t dest_len = wcslen(dest);
  size_t i;

  for (i = 0 ; i < n && src[i] != L'\0' ; i++)
    dest[dest_len + i] = src[i];
  dest[dest_len + i] = L'\0';

  return dest;
}

/* Warning: read considerations about malloc() in Frama-C */
#include "stdlib.h"
wchar_t *wcsdup(const wchar_t *ws)
{
  size_t l = wcslen(ws) + 1;
  wchar_t *p = malloc(sizeof(wchar_t) * l);
  if (!p) {
    errno = ENOMEM;
    return 0;
  }
  wmemcpy(p, ws, l);
  return p;
}

#include "stdarg.h"
#include "__fc_scanf_stub_helper.h"

int __fc_v_wscanf(const wchar_t * restrict format, va_list arg) {
  const wchar_t *p = format;
  char conversion_counter = 0;
  while (*p) {
    if (*p == L'%') {
      enum length_modifier lm = NONE;
      wchar_t asterisks = 0;
      p++;
      if (*p == L'%') {
        break;
      }
      // skip any flags
      while (1) {
        switch (*p) {
        case L'-':
        case L'+':
        case L' ':
        case L'#':
        case L'0':
          break;
        default:
          goto post_flags;
        }
        p++;
      }
    post_flags:
      // skip field width
      while (*p >= L'0' && *p <= L'9') {
        p++;
      }
      // special field width
      if (*p == L'*') {
        asterisks++;
        p++;
      }
      if (*p == L'.') {
        // skip precision
        p++;
        while (*p >= L'0' && *p <= L'9') {
          p++;
        }
        // special precision
        if (*p == L'*') {
          asterisks++;
          p++;
        }
      }
      // length modifier
      switch (*p) {
      case L'h':
        p++;
        if (*p == L'h') {
          p++;
          lm = HH;
        } else {
          lm = H;
        }
        break;
      case L'l':
        p++;
        if (*p == L'l') {
          p++;
          lm = LL;
        } else {
          lm = L;
        }
        break;
      case L'j':
        p++;
        lm = J;
        break;
      case L'z':
        p++;
        lm = Z;
        break;
      case L't':
        p++;
        lm = T;
        break;
      case L'L':
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
      case L'd':
      case L'i':
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
      case L'o':
      case L'u':
      case L'x':
      case L'X':
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
      case L'f':
      case L'F':
      case L'e':
      case L'E':
      case L'g':
      case L'G':
      case L'a':
      case L'A':
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
      case L'c':
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
      case L's':
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
      case L'n':
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

int _wscanf_possible_errors[] = {
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

#define N_WSCANF_ERRORS (sizeof(_wscanf_possible_errors)/sizeof(int))

int vfwscanf(FILE * restrict stream, const wchar_t * restrict format, va_list arg) {
  if (Frama_C_interval(0, 1)) { // simulate an error
    errno = _wscanf_possible_errors[Frama_C_interval(0, N_WSCANF_ERRORS-1)];
    return EOF;
  }
  return __fc_v_wscanf(format, arg);
}

int vwscanf(const wchar_t * restrict format, va_list arg) {
  if (Frama_C_interval(0, 1)) { // simulate an error
    errno = _wscanf_possible_errors[Frama_C_interval(0, N_WSCANF_ERRORS-1)];
    return EOF;
  }
  return __fc_v_wscanf(format, arg);
}

int vswscanf(const wchar_t * restrict ws, const wchar_t * restrict format, va_list arg) {
  //@ check valid_read_wstring(ws);
  if (Frama_C_interval(0, 1)) { // simulate an error
    errno = _wscanf_possible_errors[Frama_C_interval(0, N_WSCANF_ERRORS-1)];
    return EOF;
  }
  return __fc_v_wscanf(format, arg);
}

__POP_FC_STDLIB
