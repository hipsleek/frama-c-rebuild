/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Utilities
// --------------------------------------------------------------------------

/**
   @packageDocumentation
   @module dome/misc/utils
 */

import type { CSSProperties } from 'react';

export type Falsy = undefined | boolean | null | '';

export type ClassSpec = string | Falsy | { [cname: string]: true | Falsy };

/**
   Utility function to merge various HTML class properties
   into a `className` property.
   Class specifications can be made of:
    - a string, interpreted as a CSS class specification
    - an object, with keys corresponding to CSS class associated
      to true of falsy value.
    - any falsy value, which is discarded

    Example of usage:

    * ```ts
    *    const className = classes(
    *       'my-base-class',
    *        condition && 'my-class-when-condition',
    *        {
    *           'my-class-1': cond-1,
    *           'my-class-2': cond-2,
    *           'my-class-3': cond-3,
    *        }
    *    );
    * ```

 */
export function classes(
  ...args: ClassSpec[]
): string {
  const buffer: string[] = [];
  args.forEach((cla) => {
    if (cla) {
      if (typeof (cla) === 'string' && cla !== '') buffer.push(cla);
      else if (typeof (cla) === 'object') {
        const cs = Object.keys(cla);
        cs.forEach((c) => { if (cla[c]) buffer.push(c); });
      }
    }
  });
  return buffer.join(' ');
}

export type StyleSpec = Falsy | CSSProperties;

/**
   Utility function to merge various CSS style properties
   into a single CSS style object.

   Each style specification can be made of a CSS object or (discarded)
   falsy values.
   Example of usage:

   * ```ts
   *    const sty = styles(
   *        { ... },
   *        cond-1 && { ... },
   *        cond-2 && { ... },
   *    );
   * ```

*/

export function styles(
  ...args: StyleSpec[]
): CSSProperties | undefined {
  let empty = true;
  let buffer = {};
  args.forEach((sty) => {
    if (sty && typeof (sty) === 'object') {
      empty = false;
      buffer = { ...buffer, ...sty };
    }
  });
  return (empty ? undefined : buffer);
}

// --------------------------------------------------------------------------
