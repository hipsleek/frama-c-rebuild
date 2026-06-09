/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- States
// --------------------------------------------------------------------------

/**
   Local & Global States
   @packageDocumentation
   @module dome/data/states
*/
import Emitter from 'events';
import React from 'react';
import isEqual from 'react-fast-compare';
import { Debug } from 'dome/system';

const D = new Debug('State');

// --------------------------------------------------------------------------
// --- State utilities
// --------------------------------------------------------------------------

/** Alias for React.useState returned */
export type RState<A> = [A, React.Dispatch<React.SetStateAction<A>>]

/** Alias to `[state,setState]` returned values */
export type State<A> = [A, (newValue: A) => void];

/** State field of an object state. */
export function keyOf<A, K extends keyof A>(
  state: State<A>,
  key: K,
): State<A[K]> {
  const [props, setProps] = state;
  return [props[key], (value: A[K]) => {
    const newProps = { ...props };
    newProps[key] = value;
    setProps(newProps);
  }];
}

/** State index of an array state. */
export function index<A>(
  state: State<A[]>,
  idx: number,
): State<A> {
  const [array, setArray] = state;
  return [array[idx], (value: A) => {
    const newArray = array.slice();
    newArray[idx] = value;
    setArray(newArray);
  }];
}

/** Log state updates in the console. */
export function debug<A>(msg: string, st: State<A>): State<A> {
  const [value, setValue] = st;
  return [value, (v) => {
    setValue(v);
    D.log(msg, v);
  }];
}

/** Purely local value. No hook, no events, just a ref. */
export function local<A>(init: A): State<A> {
  const ref = { current: init };
  return [ref.current, (v) => { ref.current = v; }];
}

// --------------------------------------------------------------------------
// --- Global States
// --------------------------------------------------------------------------

const UPDATE = 'dome.states.update';

/** Cross-component State. */
export class GlobalState<A> {

  private value: A;
  private emitter: Emitter;

  constructor(initValue: A) {
    this.value = initValue;
    this.emitter = new Emitter();
    this.emitter.setMaxListeners(200);
    this.getValue = this.getValue.bind(this);
    this.setValue = this.setValue.bind(this);
  }

  /** Current state value. */
  getValue(): A { return this.value; }

  /** Notify callbacks on change. By default, changed are detected
      by using _deep_ structural comparison, using `react-fast-compare`
      comparison.
      @param value the new value of the state
      @param forced when set to `true`, notify callbacks without comparison.
  */
  setValue(value: A, forced = false): void {
    if (forced || !isEqual(value, this.value)) {
      this.value = value;
      this.emitter.emit(UPDATE, value);
    }
  }

  /** Callback Emitter. */
  on(callback: (value: A) => void): void {
    this.emitter.on(UPDATE, callback);
  }

  /** Callback Emitter. */
  off(callback: (value: A) => void): void {
    this.emitter.off(UPDATE, callback);
  }

}

/** React Hook, similar to `React.useState()`.
    Assignments to the global state also update _all_
    its associated hooks and listeners. */
export function useGlobalState<A>(s: GlobalState<A>): State<A> {
  const [current, setCurrent] = React.useState<A>(s.getValue);
  React.useEffect(() => {
    s.on(setCurrent);
    return () => s.off(setCurrent);
  }, [s]);
  return [current, s.setValue];
}

// --------------------------------------------------------------------------
