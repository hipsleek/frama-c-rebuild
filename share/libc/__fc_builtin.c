/**************************************************************************/
/*                                                                        */
/*  SPDX-License-Identifier LGPL-2.1                                      */
/*  Copyright (C)                                                         */
/*  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)  */
/*                                                                        */
/**************************************************************************/

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_builtin.h"
#include "limits.h"

/* Those builtins implementations could probably be removed entirely for
   Value, as the spec is informative enough. There remains a slight difference
   with Frama_C_float/double_interval and +0./-0., because the specification
   is not sufficient to to exclude -0. when requiring >= +0. */

int volatile Frama_C_entropy_source;

// Additional entropy sources for some interval functions
unsigned short volatile __fc_unsigned_short_entropy;
unsigned int volatile __fc_unsigned_int_entropy;
long volatile __fc_long_entropy;
unsigned long volatile __fc_unsigned_long_entropy;
long long volatile __fc_long_long_entropy;
unsigned long long volatile __fc_unsigned_long_long_entropy;
size_t volatile __fc_size_t_entropy;
intmax_t volatile __fc_intmax_t_entropy;
uintmax_t volatile __fc_uintmax_t_entropy;
ptrdiff_t volatile __fc_ptrdiff_t_entropy;
wint_t volatile __fc_wint_t_entropy;

//@ assigns Frama_C_entropy_source \from Frama_C_entropy_source;
void Frama_C_update_entropy(void) {
  Frama_C_entropy_source = Frama_C_entropy_source;
}

void Frama_C_make_unknown(char *p, size_t l) {
  Frama_C_update_entropy();
  for (size_t i = 0; i < l; i++) {
    p[i] = Frama_C_entropy_source;
  }
}

void Frama_C_make_unknown_wchar(wchar_t *p, size_t l) {
  Frama_C_update_entropy();
  for (size_t i = 0; i < l; i++) {
    p[i] = Frama_C_entropy_source;
  }
}

int Frama_C_nondet(int a, int b)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? a : b;
}

void *Frama_C_nondet_ptr(void *a, void *b)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? a : b;
}

int Frama_C_interval(int min, int max)
{
  int r, aux;
  Frama_C_update_entropy();
  aux = Frama_C_entropy_source;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

char Frama_C_char_interval(char min, char max)
{
  return (char)Frama_C_interval(min, max);
}

short Frama_C_short_interval(short min, short max)
{
  return (short)Frama_C_interval(min, max);
}

float Frama_C_float_interval(float min, float max)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? min : max;
}

double Frama_C_double_interval(double min, double max)
{
  Frama_C_update_entropy();
  return Frama_C_entropy_source ? min : max;
}

unsigned char Frama_C_unsigned_char_interval(unsigned char min, unsigned char max)
{
  return (unsigned char)Frama_C_interval(min, max);
}

unsigned short Frama_C_unsigned_short_interval(unsigned short min, unsigned short max)
{
  unsigned short r, aux;
  Frama_C_update_entropy();
  aux = __fc_unsigned_short_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

unsigned int Frama_C_unsigned_int_interval(unsigned int min, unsigned int max)
{
  unsigned int r, aux;
  Frama_C_update_entropy();
  aux = __fc_unsigned_int_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

long Frama_C_long_interval(long min, long max)
{
  long r, aux;
  Frama_C_update_entropy();
  aux = __fc_long_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

unsigned long Frama_C_unsigned_long_interval(unsigned long min, unsigned long max)
{
  unsigned long r, aux;
  Frama_C_update_entropy();
  aux = __fc_unsigned_long_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

long long Frama_C_long_long_interval(long long min, long long max)
{
  long long r, aux;
  Frama_C_update_entropy();
  aux = __fc_long_long_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

unsigned long long Frama_C_unsigned_long_long_interval(unsigned long long min, unsigned long long max)
{
  unsigned long long r, aux;
  Frama_C_update_entropy();
  aux = __fc_unsigned_long_long_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

size_t Frama_C_size_t_interval(size_t min, size_t max)
{
  size_t r, aux;
  Frama_C_update_entropy();
  aux = __fc_size_t_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

intmax_t Frama_C_intmax_t_interval(intmax_t min, intmax_t max)
{
  intmax_t r, aux;
  Frama_C_update_entropy();
  aux = __fc_intmax_t_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

uintmax_t Frama_C_uintmax_t_interval(uintmax_t min, uintmax_t max)
{
  uintmax_t r, aux;
  Frama_C_update_entropy();
  aux = __fc_uintmax_t_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

ptrdiff_t Frama_C_ptrdiff_t_interval(ptrdiff_t min, ptrdiff_t max)
{
  ptrdiff_t r, aux;
  Frama_C_update_entropy();
  aux = __fc_ptrdiff_t_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

wint_t Frama_C_wint_t_interval(wint_t min, wint_t max)
{
  wint_t r, aux;
  Frama_C_update_entropy();
  aux = __fc_wint_t_entropy;
  if (aux >= min && aux <= max)
    r = aux;
  else
    r = min;
  return r;
}

/*@
  terminates \false;
  assigns \nothing;
  ensures never_terminates: \false;
  exits always_exits: \true;
 */
extern void __builtin_abort(void) __attribute__((noreturn)); // GCC builtin

void Frama_C_abort(void)
{
  __builtin_abort();
}


__POP_FC_STDLIB
