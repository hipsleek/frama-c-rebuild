/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* eslint-disable @typescript-eslint/no-explicit-any, no-console */

// --------------------------------------------------------------------------
// --- JSON Utilities
// --------------------------------------------------------------------------

/**
   Safe JSON utilities.
   @packageDocumentation
   @module dome/data/json
*/

import { Debug } from 'dome/system';

const D = new Debug('Dome.json');

export type json =
  undefined | null | boolean | number | string |
  json[] | { [key: string]: json };

export type jobject = { [key: string]: json };


/* Parsing exceptions.
   Stores a string telling what was expected and a json of what have been
   given */

export class JsonError extends Error {
  given: json;

  constructor(given: json) {
    super("wrong json data type");
    this.name = this.constructor.name;
    this.given = given;
  }
}

export class JsonTypeError extends JsonError {
  expected: string;

  constructor(expected: string, given: json) {
    super(given);
    this.expected = expected;
  }

  toString(): string {
    return `expected ${this.expected} but given ${JSON.stringify(this.given)}`;
  }
}

class JsonUnionError extends JsonError {
  #errors: JsonError[];

  constructor(errors: JsonError[], given: json) {
    super(given);
    this.#errors = errors;
  }

  get errors(): JsonTypeError[] {
    let errorsDeep: JsonTypeError[] = [];
    this.#errors.forEach(e => {
      if (e instanceof JsonTypeError) {
        errorsDeep.push(e);
      }
      else if (e instanceof JsonUnionError) {
        errorsDeep = errorsDeep.concat(e.errors);
      }
    });
    return errorsDeep;
  }

  toString(): string {
    return 'none of the union options are valid.\n' +
      this.errors.map((e) => `  - ${e}`).join('\n');
  }
}

/**
   Parse without _revivals_.
   Returned data is guaranteed to have only [[json]] type.
   If an error occurs and `noError` is set to `true`,
   the function returns `undefined` and logs the error in console
   (DEVEL mode only).
 */
export function parse(text: string, noError = false): json {
  if (noError) {
    try {
      return JSON.parse(text);
    }
    catch (err) {
      if (err instanceof JsonError) {
        D.error(err);
        return undefined;
      }
      else {
        throw err;
      }
    }
  } else
    return JSON.parse(text);
}

/**
   Export JSON (or any data) as a compact string.
*/
export function stringify(js: any): string {
  return JSON.stringify(js, undefined, 0);
}

/**
   Export JSON (or any data) as a string with indentation.
 */
export function pretty(js: any): string {
  return JSON.stringify(js, undefined, 2);
}

// --------------------------------------------------------------------------
// --- Decoder & Encoder types
// --------------------------------------------------------------------------

/** Decoder for values of type `D`.
    You can abbreviate `Safe<D | undefined>` with `Loose<D>`. */
export interface Decoder<D> {
  (js?: json): D;
}

/**
   Encoder for value of type `D`.
   In most cases, you only need [[identity]].
 */
export interface Encoder<D> {
  (v: D): json;
}

/** Can be used for most encoders. */
export function identity<A>(v: A): A { return v; }

// --------------------------------------------------------------------------
// --- Primitives
// --------------------------------------------------------------------------

/** 'null' or throws JsonError. */
export const jNull: Decoder<null> = (js: json) => {
  if (js === null) {
    return null;
   }
   else {
    throw new JsonTypeError("null", js);
   }
};

/** Identity. */
export const jAny: Decoder<json> = (js: json) => js;

/** JSON Object or throws JsonError. */
export const jObj: Decoder<jobject> = (js: json) => {
  if (typeof js === 'object' && !Array.isArray(js) && js !== null) {
    return js;
  }
  else {
    throw new JsonTypeError("object", js);
  }
};

/** Primitive JSON number or throws JsonError. */
export const jNumber: Decoder<number> = (js: json) => {
  if (typeof js === 'number' && !Number.isNaN(js)) {
    return js;
  }
  else {
    throw new JsonTypeError("number", js);
  }
};

/** JSON number in range with default value. */
export function jRange(
  min: number,
  max: number,
  def: number
): Decoder<number> {
  return (js: json): number => {
    if (typeof js === 'number')
      return Math.max(Math.min(js, max), min);
    return def;
  };
}

/** Primitive JSON number if it is an integer or throws JsonError. */
export const jInt: Decoder<number> = (js: json) => {
  if (typeof js === 'number' && Number.isInteger(js)) {
    return js;
  }
  else {
    throw new JsonTypeError("integer", js);
  }
};

/** JSON number in range with default value. */
export function jRangeInt(
  min: number,
  max: number,
  def: number
): Decoder<number> {
  return (js: json): number => {
    if (typeof js === 'number')
      return Math.max(Math.min(Math.round(js), max), min);
    return def;
  };
}

/** Primitive JSON number or `0`. */
export const jZero: Decoder<number> = (js: json) => (
  typeof js === 'number' && !Number.isNaN(js) ? js : 0
);

/** Primitive JSON boolean or throws JsonError. */
export const jBoolean: Decoder<boolean> = (js: json) => {
  if (typeof js === 'boolean') {
    return js;
  }
  else {
    throw new JsonTypeError("boolean", js);
  }
};

/** Primitive JSON boolean or `true`. */
export const jTrue: Decoder<boolean> = (js: json) => (
  typeof js === 'boolean' ? js : true
);

/** Primitive JSON boolean or `false`. */
export const jFalse: Decoder<boolean> = (js: json) => (
  typeof js === 'boolean' ? js : false
);

/** Primitive JSON string or throws JsonError. */
export const jString: Decoder<string> = (js: json) => {
    if (typeof js === 'string') {
      return js;
    }
    else {
      throw new JsonTypeError("string", js);
    }
  };


/** JSON constant.
    Capture the tag or throw JsonError.
    Can be used with [[jUnion]], although [[jEnum]]
    might be more efficient.
*/
export function jTag<A>(tg: A): Decoder<A> {
  return (js: json) => {
    if (Object.is(js, tg)) {
      return tg;
    }
    else {
      throw new JsonTypeError(`"${tg}"`, js);
    }
  };
}

/**
   Lookup tags in a dictionary, throw JsonError if the tag is not found.
   Can be used directly for enum types, eg. `jEnum(myEnumType)`.
 */
export function jEnum<A>(d: { [tag: string]: A }): Decoder<A> {
  return (js: json) => {
    if (typeof js === 'string' && js in d) {
      return d[js];
    }
    else {
      const tags = Object.keys(d).map((tg) => `"${tg}"`);
      throw new JsonTypeError(tags.join(' | '), js);
    }
  };
}

/**
   One of the enumerated _constants_ or throws JsonError.
   The typechecker will prevent you from listing values that are not in
   type `A`. However, it will not protect you from missing constants in `A`.
*/
export function jTags<A extends string | number>(...values: A[]): Decoder<A> {
  const m = new Set<string | number>();
  values.forEach((v) => m.add(v));
  return (js: json) => {
    if ((typeof js === 'string' || typeof js === 'number') && m.has(js)) {
      return js as A; // m.has(js) implies js extends A
    }
    else {
      const tags = values.map((tg) => typeof tg === 'string' ? `"${tg}"` : tg);
      throw new JsonTypeError(tags.join(' | '), js);
    }
  };
}

/**
   Force returning `undefined` or a default value for `undefined` _or_ `null`
   JSON input.
 */
export function jOption<A>(fn: Decoder<A>, defaultValue?: A)
  : Decoder<A | undefined> {
  return (js: json) => (
    js === undefined || js === null ? defaultValue : fn(js)
  );
}

/**
   Provide a fallback value in case of a JsonError.
 */
export function jCatch<A>(fn: Decoder<A>, fallBack: A): Decoder<A> {
  return (js: json) => {
    try {
      return fn(js);
    }
    catch (err) {
      if (err instanceof JsonError) {
        return fallBack;
      }
      else {
        throw err;
      }
    }
  };
}

/**
   Converts objects to Maps.
 */
export function jMap<A>(fn: Decoder<A>): Decoder<Map<string, A>> {
  return (js: json) => {
    if (js !== null && typeof js === 'object' && !Array.isArray(js)) {
      const m = new Map<string, A>();
      for (const k of Object.keys(js)) {
        m.set(k, fn(js[k]));
      }
      return m;
    }
    else {
      throw new JsonTypeError('object', js);
    }
  };
}

/**
   Converts dictionaries to maps.
 */
export function eMap<A>(fn: Encoder<A>): Encoder<Map<string, undefined | A>> {
  return (m) => {
    const js: json = {};
    m.forEach((v, k) => {
      if (v !== undefined) {
        const u = fn(v);
        if (u !== undefined) js[k] = u;
      }
    });
    return js;
  };
}

/**
   Apply the decoder on each item of a JSON array or throw JsonError if
   the decoded json is not an array.
 */
export function jArray<A>(fn: Decoder<A>): Decoder<A[]> {
  return (js: json) => {
    if (Array.isArray(js)) {
      return js.map(fn);
    }
    else {
      throw new JsonTypeError('array', js);
    }
  };
}

/**
    Apply the decoder on each item of a JSON array, discarding
    all JsonError exceptions from decoded items. The decoded JSON must still be
    an array, otherwise a JsonError is raised.
  */
export function jList<A>(fn: Decoder<A>): Decoder<A[]> {
  return (js: json) => {
    if (Array.isArray(js)) {
      const buffer = [];
      for (const element of js) {
        try {
          buffer.push(fn(element));
        }
        catch (err) {
          if (err instanceof JsonError) {
            continue;
          }
          else {
            throw err;
          }
        }
      }
      return buffer;
    }
    else {
      throw new JsonTypeError('array', js);
    }
  };
}

/**
   Exports all non-undefined elements.
 */
export function eList<A>(fn: Encoder<A>): Encoder<(A | undefined)[]> {
  return (m) => {
    const js: json[] = [];
    m.forEach((v) => {
      if (v !== undefined) {
        const u = fn(v);
        if (u !== undefined) js.push(u);
      }
    });
    return js;
  };
}

/** Apply a pair of decoders to JSON pairs, or throw JsonError if not a pair. */
export function jPair<A, B>(
  fa: Decoder<A>,
  fb: Decoder<B>,
): Decoder<[A, B]> {
  return (js: json) => {
    if (Array.isArray(js) && js.length === 2) {
      return [fa(js[0]) as A, fb(js[1]) as B];
    }
    else {
      throw new JsonTypeError('[A, B]', js);
    }
  };
}

/** Similar to [[jPair]]. */
export function jTriple<A, B, C>(
  fa: Decoder<A>,
  fb: Decoder<B>,
  fc: Decoder<C>,
): Decoder<[A, B, C]> {
  return (js: json) => {
    if (Array.isArray(js) && js.length === 3) {
      return [fa(js[0]), fb(js[1]), fc(js[2])];
    }
    else {
      throw new JsonTypeError('[A, B, C]', js);
    }
  };
}

/** Similar to [[jPair]]. */
export function jTuple4<A, B, C, D>(
  fa: Decoder<A>,
  fb: Decoder<B>,
  fc: Decoder<C>,
  fd: Decoder<D>,
): Decoder<[A, B, C, D]> {
  return (js: json) => {
    if (Array.isArray(js) && js.length === 4) {
      return [fa(js[0]), fb(js[1]), fc(js[2]), fd(js[3])];
    }
    else {
      throw new JsonTypeError('[A, B, C, D]', js);
    }
  };
}

/** Similar to [[jPair]]. */
export function jTuple5<A, B, C, D, E>(
  fa: Decoder<A>,
  fb: Decoder<B>,
  fc: Decoder<C>,
  fd: Decoder<D>,
  fe: Decoder<E>,
): Decoder<[A, B, C, D, E]> {
  return (js: json) => {
    if (Array.isArray(js) && js.length === 5) {
      return [fa(js[0]), fb(js[1]), fc(js[2]), fd(js[3]), fe(js[4])];
    }
    else {
      throw new JsonTypeError('[A, B, C, D, E]', js);
    }
  };
}

/**
   Decoders for each property of object type `A`.
   Optional fields in `A` can use jOption
*/
export type Props<A> = {
  [P in (keyof A & string)]: Decoder<A[P]>;
};

/**
   Decode an object given the decoders of its fields.
   Returns `undefined` for non-object JSON.
 */
export function jObject<A extends object>(decoders: Props<A>): Decoder<A> {
  return (js: json) => {
    if (js !== null && typeof js === 'object' && !Array.isArray(js)) {
      const buffer: Partial<A> = {};
      for (const k of Object.keys(decoders) as (keyof A & string)[]) {
        buffer[k] = decoders[k](js[k]);
      }
      return buffer as A; // All fields should be present
    }
    else {
      throw new JsonTypeError('object', js);
    }
  };
}

/**
   Returns the first decoder result that does not fail with a JsonError.
 */
export function jUnion<A>(...cases: Decoder<A>[]): Decoder<A> {
  return (js: json) => {
    const errors: JsonError[] = [];
    for (const fv of cases) {
      try {
        return fv(js);
      }
      catch (err) {
        if (err instanceof JsonError) {
          errors.push(err);
          continue;
        }
        else {
          throw err;
        }
      }
    }
    throw new JsonUnionError(errors, js);
  };
}

/**
   Encoders for each property of object type `A`.
*/
export type EProps<A> = {
  [P in keyof A]?: Encoder<A[P]>;
};

/**
   Encode an object given the provided encoders by fields.
   The exported JSON object has only original
   fields with some specified encoder.
 */
export function eObject<A>(fp: EProps<A>): Encoder<A> {
  return (m: A) => {
    const js: json = {};
    const keys = Object.keys(fp);
    keys.forEach((k) => {
      const fn = fp[k as keyof A];
      if (fn !== undefined) {
        const fv = m[k as keyof A];
        if (fv !== undefined) {
          const r = fn(fv);
          if (r !== undefined) js[k] = r;
        }
      }
    });
    return js;
  };
}

// Intentionnaly internal and only declared
// eslint-disable-next-line @typescript-eslint/no-unused-vars
declare const tag: unique symbol;

/** Phantom type. */
export type phantom<K, A> = A & { tag: K };

export function forge<K, A>(_tag: K, data: A): phantom<K, A> {
  return data as phantom<K, A>;
}

/** String key with kind.
    Can be used as a `string` but shall be created with [forge]. */
export type key<K> = phantom<K, string>;

/** Number index with kind.
    Can be used as a `number` but shall be created with [forge]. */
export type index<K> = phantom<K, number>;

/** Decoder for `key<K>` strings. */
export function jKey<K>(kd: K): Decoder<key<K>> {
  return (js: json) => {
    if (typeof js === 'string') {
      return forge(kd, js);
    }
    else {
      throw new JsonTypeError(`key<${kd}>`, js);
    }
  };
}

/** Decoder for `index<K>` numbers. */
export function jIndex<K>(kd: K): Decoder<index<K>> {
  return (js: json) => {
    if (typeof js === 'number') {
      return forge(kd, js);
    }
    else {
      throw new JsonTypeError(`index<${kd}>`, js);
    }
  };
}

/** Dictionaries. */
export type dict<A> = { [key: string]: A };

/**
   Decode a JSON dictionary, discarding all inconsistent entries.
   If the JSON contains no valid entry, still returns `{}`.
*/
export function jDict<A>(fn: Decoder<A>): Decoder<dict<A>> {
  return (js: json) => {
    const buffer: dict<A> = {};
    if (js !== null && typeof js === 'object' && !Array.isArray(js)) {
      for (const k of Object.keys(js)) {
        try {
          const fv = fn(js[k]);
          if (fv !== undefined) {
            buffer[k] = fv;
          }
        }
        catch (err) {
          if (err instanceof JsonError) {
            continue;
          }
          else {
            throw err;
          }
        }
      }
    }
    return buffer;
  };
}


/**
   Encode a dictionary into JSON, discarding all inconsistent entries.
   If the dictionary contains no valid entry, still returns `{}`.
*/
export function eDict<A>(fn: Encoder<A>): Encoder<dict<A>> {
  return (d: dict<A>) => {
    const js: json = {};
    const keys = Object.keys(d);
    keys.forEach((k) => {
      const fv = d[k];
      if (fv !== undefined) {
        const fr = fn(fv);
        if (fr !== undefined) js[k] = fr;
      }
    });
    return js;
  };
}

// --------------------------------------------------------------------------
