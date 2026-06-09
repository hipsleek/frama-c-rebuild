/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
   Safe ARRAY utilities.
   @packageDocumentation
   @module dome/data/arrays
*/

/** Swaps items at index i and j if they are both in range. */
export function swap<A>(ls: A[], a: number, b: number): A[] {
  const n = ls.length;
  if (a === b || 0 > a || a >= n || 0 > b || b >= n) return ls;
  const [i, j] = a < b ? [a, b] : [b, a];
  return ls.slice(0, i).concat(ls.slice(i + 1, j + 1), ls[i], ls.slice(j + 1));
}

/** Remove item at index i when in range. */
export function removeAt<A>(ls: A[], k: number): A[] {
  return 0 <= k && k < ls.length ? ls.slice(0, k).concat(ls.slice(k + 1)) : ls;
}

/** Insert an item at index i when in range or off-by-one. */
export function insertAt<A>(ls: A[], id: A, k: number): A[] {
  return 0 <= k && k <= ls.length ? ls.slice(0, k).concat(id, ls.slice(k)) : ls;
}


export type Indexed = { key: unknown; }

/** Merges elements of the second array into matching elements of the first.
    The function passed as parameter is used to extract the key from both a1 and
    a2. Elements of a1 are merged with an element of a2 with a matching key if
    one exists.
    The length and order of the first array is preserved. Elements of the second
    array matching no element of the first are ignored. */
export function merge<K, A extends K, B extends K>(
  a1: A[],
  a2: B[],
  f: (x : K) => unknown,
): (A & (object | B))[] {
  const dict = new Map(a2.map(x2 => [f(x2), x2])); // maps f(x2) to x2
  return a1.map(x1 => ({ ...x1, ...(dict.get(f(x1))) }));
}

/** Maps a function through an array and returns the first computed value that
    is not undefined. */
export type Maybe<A> = A | undefined;
export function first<X, R>(xs: X[], fn: (x: X) => Maybe<R>): Maybe<R> {
  for (const x of xs) { const r = fn(x); if (r) return r; }
  return undefined;
}
