/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Formatting Utilities
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/misc/format
 */

/**
   Formats a duration, specified in seconds, into hour, minutes, seconds,
   milliseconds or nanoseconds, depending on range.

   Negative or null durations are reported by `'0'`.

   For instance, returns `'250ms'` for an input time of `.25`.
 */
export function duration(time : number) : string {
  if (time <= 0.0) return '0';
  if (time < 1.0e-3) return `${Math.round(time * 1.0e6)}µs`;
  if (time < 1.0) return `${Math.round(time * 1.0e3)}ms`;
  if (time < 60) return `${Math.round(time)}s`;
  if (time < 3600) return `${Math.round(time / 60)}m`;
  const h = Math.round(time / 3600);
  const r = time - h * 3600;
  const m = Math.round(r / 60);
  return `${h}h ${m}m`;
}

// --------------------------------------------------------------------------
