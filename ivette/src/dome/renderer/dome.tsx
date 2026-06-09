/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/**
   Dome Application (Renderer Process)

   This modules manages your main application window
   and its interaction with the main process.

   Example:

   ```ts
   // File 'src/renderer/index.js':
   import Application from './Application.js' ;
   Dome.setContent( Application );
   ```

   @packageDocumentation
   @module dome
 */

import _ from 'lodash';
import React from 'react';
import Emitter from 'events';
import { createRoot } from 'react-dom/client';
import { ipcRenderer } from 'electron';

import SYS, * as System from 'dome/system';
import { State, GlobalState, useGlobalState } from './data/states';
import * as Json from 'dome/data/json';
import * as Settings from 'dome/data/settings';

import * as MenuBar from '../main/menubar';
import * as path from 'path';

import './dark.css';
import './light.css';
import './style.css';

// --------------------------------------------------------------------------
// --- Context
// --------------------------------------------------------------------------

// main window focus
let windowFocus = true;

function setContextAppNode(): HTMLElement | null {
  const node = document.getElementById('root');
  if (node) {
    const os = System.platform;
    const focus = windowFocus ? 'active' : 'inactive';
    node.className =
      `dome-container dome-platform-${os} dome-window-${focus}`;
  }
  return node;
}

// --------------------------------------------------------------------------
// --- Helpers
// --------------------------------------------------------------------------

/** DEVEL is configured to be `'true'` when in development mode. */
export const { DEVEL, Debug } = System;

export type PlatformKind = 'linux' | 'macos' | 'windows';

/** System platform. */
export const platform: PlatformKind = (System.platform as PlatformKind);

// --------------------------------------------------------------------------
// --- User's Directories
// --------------------------------------------------------------------------

let loadedPaths = false;
const remoteAppPaths: { [index: string]: string } = {};

function getPath(k: string): string {
  if (!loadedPaths) {
    loadedPaths = true;
    Object.assign(remoteAppPaths, ipcRenderer.sendSync('dome.app.paths'));
  }
  return remoteAppPaths[k];
}

/** Returns user's home directory. */
export function getHome(): string { return getPath('home'); }

/** Returns user's desktop directory. */
export function getDesktop(): string { return getPath('desktop'); }

/** Returns user's documents directory. */
export function getDocuments(): string { return getPath('documents'); }

/** Returns user's downloads directory. */
export function getDownloads(): string { return getPath('downloads'); }

/** Returns temporary directory. */
export function getTempDir(): string { return getPath('temp'); }

/** Working directory (Application Window). */
export function getWorkingDir(): string { return System.getWorkingDir(); }

/** Current process ID.. */
export function getPID(): number { return System.getPID(); }

/** Path to application static resources. */
export function getStatic(file?: string): string {
  return file ? System.join(__dirname, file) : __dirname;
}

// --------------------------------------------------------------------------
// --- Application Emitter
// --------------------------------------------------------------------------

/** Typed Dome Event.

   To register an event with no argument, simply use `new Event('myEvent')`.
 */
export class Event<A = void> {

  private name: string;

  constructor(name: string) {
    this.name = name;
    this.emit = this.emit.bind(this);
  }

  on(callback: (arg: A) => void): void {
    System.emitter.on(this.name, callback);
  }

  off(callback: (arg: A) => void): void {
    System.emitter.off(this.name, callback);
  }

  /**
     Notify all listeners with the provided argument.
     This methods is bound to the event, so you may use `myEvent.emit`
     as a callback function, instead of eg. `(arg) => myEvent.emit(arg)`.
  */
  emit(arg: A): void {
    System.emitter.emit(this.name, arg);
  }

  /**
     Number of currently registered listeners.
   */
  listenerCount(): number {
    return System.emitter.listenerCount(this.name);
  }

}

/** Custom React Hook on event. */
export function useEvent<A>(
  evt: undefined | null | Event<A>,
  callback: (arg: A) => void,
): void {
  React.useEffect(() => {
    if (evt) {
      evt.on(callback);
      return () => evt.off(callback);
    }
    return undefined;
  }, [evt, callback]);
}

/** Custom React Hook on Node Emitters. */
export function useEmitter(
  emitter: undefined | null | Emitter,
  event: undefined | null | string,
  callback: () => void,
): void {
  return React.useEffect((): (undefined | (() => void)) => {
    if (emitter && event) {
      emitter.on(event, callback);
      return () => emitter.off(event, callback);
    }
    return undefined;
  }, [emitter, event, callback]);
}

// --------------------------------------------------------------------------
// --- Application Events
// --------------------------------------------------------------------------

/**
   Dome update event.
   It is emitted when a general re-rendering is required, typically when
   the window frame is resized.
   You can use it for your own components as an easy-to-use global
   re-render event.
 */
export const update = new Event('dome.update');

/**
   Dome reload event.
   It is emitted when the entire window is reloaded.
 */
export const reload = new Event('dome.reload');
ipcRenderer.on('dome.ipc.reload', () => reload.emit());

/**
   Dome « Find » event. Triggered by [Cmd+F] and [Edit > Find] menu.
 */
export const find = new Event('dome.find');
ipcRenderer.on('dome.ipc.find', () => find.emit());

/** Command-line arguments event handler. */
export function onCommand(
  job: (argv: string[], workingDir: string) => void,
): void {
  System.emitter.on('dome.command', job);
}

ipcRenderer.on('dome.ipc.command', (_event, argv, wdir) => {
  SYS.setCommandLine(argv, wdir);
  System.emitter.emit('dome.command', argv, wdir);
});

/** Window Settings event.
    Emitted when window settings are reset or restored. */
export const windowSettings = new Event(Settings.window);

/** Global Settings event.
    Emitted when global settings are updated. */
export const globalSettings = new Event(Settings.global);

// --------------------------------------------------------------------------
// --- Closing
// --------------------------------------------------------------------------

ipcRenderer.on('dome.ipc.closing', async (_event, wid: number) => {
  await System.doExit();
  ipcRenderer.send('dome.ipc.closing.done', wid);
});

/** Register a callback to be executed when the window is closing. */
export function atExit(callback: () => (void | Promise<void>)): void {
  System.atExit(callback);
}

// --------------------------------------------------------------------------
// --- Focus Management
// --------------------------------------------------------------------------

/** Window focus event. */
export const focus = new Event<boolean>('dome.focus');

/** Current focus state of the main window. See also [[useWindowFocus]]. */
export function isFocused(): boolean { return windowFocus; }

ipcRenderer.on('dome.ipc.focus', (_sender, value) => {
  windowFocus = value;
  setContextAppNode();
  focus.emit(value);
});

/** Return the current window focus. See [[isFocused]]. */
export function useWindowFocus(): boolean {
  useUpdate(focus);
  return windowFocus;
}

// --------------------------------------------------------------------------
// --- Web Navigation
// --------------------------------------------------------------------------

/**
   DOM href events for internal URLs.

   This event is emitted whenever some `<a href/>` DOM element
   is clicked with an internal link. External links will be automatically
   opened with the user's default Web navigator.
 */
export const navigate = new Event<string>('dome.href');

ipcRenderer.on('dome.ipc.href', (_sender, href) => navigate.emit(href));

// --------------------------------------------------------------------------
// --- Window Management
// --------------------------------------------------------------------------

export function isApplicationWindow(): boolean {
  return process.argv.includes(SYS.WINDOW_APPLICATION_ARGV);
}

export function isPreferencesWindow(): boolean {
  return process.argv.includes(SYS.WINDOW_PREFERENCES_ARGV);
}

// --------------------------------------------------------------------------
// --- Window Title
// --------------------------------------------------------------------------

/** Sets the modified status of the window-frame flag.
    User feedback is platform dependent. */
export function setModified(modified = false): void {
  ipcRenderer.send('dome.ipc.window.modified', modified);
}

/** Sets the window-frame title. */
export function setTitle(title: string): void {
  ipcRenderer.send('dome.ipc.window.title', title);
}

// --------------------------------------------------------------------------
// --- Window Modal Container
// --------------------------------------------------------------------------

export type ModalInfos = {
  content: React.ReactNode,
  /**
   * onClose will be called before the modal is closed.
   * The function must return false if the modal should not be closed,
   * true otherwise.
   */
  onClose?: () => boolean | Promise<boolean>
}
export const modal = new GlobalState<ModalInfos | undefined>(undefined);

function ModalContainer(): JSX.Element | null {
  const [ modalInfos, ] = useGlobalState(modal);

  if(modalInfos?.content === undefined) return null;
  return (
    <div className="dome-xModal-overlay" >
      <div
        className="dome-xModal"
        onClick={(event) => event.stopPropagation()}
      >
        {modalInfos.content}
      </div>
    </div>
  );
}

// --------------------------------------------------------------------------
// --- Window Container
// --------------------------------------------------------------------------

function setContainer(
  Component: React.FunctionComponent | React.ComponentClass,
): void {
  Settings.synchronize();
  const appNode = setContextAppNode();
  if (appNode) {
    createRoot(appNode).render(<><Component /><ModalContainer /></>);
  }
  else
    // eslint-disable-next-line no-console
    console.error('[Dome] root element #root not found.');
}

// --------------------------------------------------------------------------
// --- Main Content
// --------------------------------------------------------------------------

/**
   Defines the user's main window content.

   Binds the component to the main window.  A `<Component/>` instance is
   generated and rendered in the `#root` window element. Its class name is set
   to `dome-platform-<platform>` with the `<platform>` set to the
   `Dome.platform` value. This class name can be used as a CSS selector for
   platform-dependent styling.

   @param Component - to be rendered in the main window
*/
export function setApplicationWindow(
  Component: React.FunctionComponent | React.ComponentClass,
): void {
  if (isApplicationWindow()) setContainer(Component);
}

// --------------------------------------------------------------------------
// --- Settings Window
// --------------------------------------------------------------------------

/**
   Defines the user's preferences window content.

   A `<Component/>` instance is generated and rendered in the `#root` window
   element. Its class name is set to `dome-platform-<platform>` with the
   `<platform>` set to the `Dome.platform` value. This class name can be used as
   a CSS selector for platform-dependent styling.

   @param Component - to be rendered in the preferences window
*/
export function setPreferencesWindow(
  Component: React.FunctionComponent | React.ComponentClass,
): void {
  if (isPreferencesWindow()) setContainer(Component);
}

// --------------------------------------------------------------------------
// --- MenuBar Management
// --------------------------------------------------------------------------

type callback = () => void;
const customItemCallbacks = new Map<string, callback>();

/**
   Create a new custom menu in the menu bar.


   This function shall be called statically, although calls from _secondary_
   windows would be ignored.

   It is also possible to call this function from the main process.

   @param label - the menu title (shall be unique)
 */
export function addMenu(label: string): void {
  ipcRenderer.send('dome.ipc.menu.addmenu', label);
}

/**
   Delete a custom menu in the menu bar.


   This function shall be called statically, although calls from _secondary_
   windows would be ignored.

   It is also possible to call this function from the main process.

   @param label - the menu title
 */
export function delSubMenu(label: string): void {
  ipcRenderer.send('dome.ipc.menu.delsubmenu', label);
}

export type MenuName = 'File' | 'Edit' | 'View' | 'Help' | string;
export type MenuItemType = 'normal' | 'separator' | 'checkbox' | 'radio';

export interface MenuItemProps {
  /** The label of the menu to insert the item in. */
  menu: MenuName;
  /** The menu item identifier. Shall be unique in the _entire_ menu bar. */
  id: string;
  /** Default is `'normal'`. */
  kind?: MenuItemType;
  /** Item label. Only optional for separators. */
  label?: string;
  /** Item is visible or not (default is `true`). */
  visible?: boolean;
  /** Enabled item (default is `true`). */
  enabled?: boolean;
  /** Item status for radio and checkbox. Default is `false`. */
  checked?: boolean;
  /** Keyboard shortcut. */
  key?: string;
  /** Callback. */
  onClick?: () => void;
}

/** Inserts a new custom item in a menu.

   The menu can be modified later with [[setMenuItem]].

   When clicked, the menu-item will also trigger a `'dome.menu.clicked'(id)`
   event on all application windows.  The item callback, if any, is invoked only
   in the process that specify it.

   Key short cuts shall be specified with the following codes: - `"Cmd+<Key>"`
   for command (MacOS) or control (Linux) key - `"Alt+<Key>"` for command+option
   (MacOS) or alt (Linux) key - `"Meta+<Key>"` for command+shift (MacOS) or
   control+alt (Linux) key

   This function shall be called statically, although calls from _secondary_
   windows would be ignored. It is also possible to call this function from the
   main process.
 */
export function addMenuItem(props: MenuItemProps): void {
  if (!props.id && props.kind !== 'separator') {
    // eslint-disable-next-line no-console
    console.error('[Dome] Missing menu-item identifier', props);
    return;
  }
  const { onClick, kind = 'normal', ...others } = props;
  if (onClick) customItemCallbacks.set(props.id, onClick);
  ipcRenderer.send('dome.ipc.menu.addmenuitem', { ...others, type: kind });
}

export interface MenuItemOptions {
  id: string;
  label?: string;
  visible?: boolean;
  enabled?: boolean;
  checked?: boolean;
  onClick?: null | (() => void);
}

/**
   Update properties of an existing menu-item.

   If an `onClick` callback is specified, it will _replace_ the previous one.
   You shall specify `null` to remove the previously registered callback
   (`undefined` callback is ignored).

   It is also possible to call this function from the main process.
 */
export function setMenuItem(options: MenuItemOptions): void {
  const { onClick, ...updates } = options;
  if (onClick === null) {
    customItemCallbacks.delete(options.id);
  } else if (onClick !== undefined) {
    customItemCallbacks.set(options.id, onClick);
  }
  ipcRenderer.send('dome.ipc.menu.setmenuitem', updates);
}

ipcRenderer.on('dome.ipc.menu.clicked', (_sender, id: string) => {
  const callback = customItemCallbacks.get(id);
  if (callback) callback();
});

// --------------------------------------------------------------------------
// --- Context Menus
// --------------------------------------------------------------------------

export interface PopupMenuItemProps {
  /** Item label. */
  label: string;
  /** Optional menu identifier. */
  id?: string;
  /** Displayed item, default is `true`. */
  display?: boolean;
  /** Enabled item, default is `true`. */
  enabled?: boolean;
  /** Checked item, default is `false`. */
  checked?: boolean;
  /** Item selection callback. */
  onClick?: (() => void);
  /** Submenu */
  submenu?: PopupMenuItem[];
}

export type PopupMenuItem = PopupMenuItemProps | 'separator';

/** Return transformed menu items with unique ids, and an index with each
 *  original item bound to the id of the transformed item. */
function transformItems(items: PopupMenuItem[])
  : [MenuBar.PopupMenuItem[], PopupMenuItemProps[]] {
  let currentId = 0;
  const indexItems: PopupMenuItemProps[] = [];
  function getItem(item: PopupMenuItem): MenuBar.PopupMenuItem {
    if (item === 'separator') return { type: 'separator' };
    const { label, display, enabled, checked } = item;
    if (item.submenu) {
      const submenu = item.submenu.map(getItem);
      return { type: 'submenu', submenu, label, display, enabled, checked };
    }
    const type = item.checked === undefined ? 'normal' : 'checkbox';
    const id = currentId++;
    indexItems.push(item);
    return { label, display, enabled, checked, type, id };
  }
  const ipcItems = items.map(getItem);
  return [ipcItems, indexItems];
}

/**
   Popup a contextual menu.

   Items can be separated by inserting a `'separator'` constant string in the
   array. Item identifier and label default to each others. Alternatively, an
   item can be specified by a single string that will be used for both its label
   and identifier. Undefined or null items are allowed (and skipped).

   The menu is displayed at the current mouse location.  The callback is called
   with the selected item identifier or label.  If the menu popup is canceled by
   the user, the callback is called with `undefined`.

   Example:

   * ```ts
   *    let myPopup = (_evt) => Dome.popupMenu([ …items… ],(id) => … );
   *    <div onRightClick={myPopup}>...</div>
   * ```

*/
export function popupMenu(
  items: PopupMenuItem[],
  callback?: (item: string | undefined) => void
): void {
  const [ipcItems, indexItems] = transformItems(items);

  ipcRenderer.invoke('dome.popup', ipcItems).then((itemId?: number) => {
    if(itemId !== undefined) {
      const { id, label, onClick } = indexItems[itemId];
      if (onClick) onClick();
      if (callback) callback(id ?? label);
    } else {
      if (callback) callback(undefined);
    }
  });
}

// --------------------------------------------------------------------------
// --- React Hooks
// --------------------------------------------------------------------------

/**
   Hook to re-render on demand (Custom React Hook).
   Returns a callback to trigger a render on demand.
*/
export function useForceUpdate(): () => void {
  const [, onTic] = React.useState(false);
  return React.useCallback(() => onTic((tac) => !tac), []);
}

/**
   Hook for a flipping boolean state.
   The updating callback can be used either as a setter or as a flipper.
 */
export function useFlipState(
  init: boolean
): [boolean, (forced?: boolean) => void] {
  const [value, setValue] = React.useState(init);
  const flipValue = React.useCallback(
    (forced?: boolean) => {
      if (forced !== undefined)
        setValue(forced);
      else
        setValue((v) => !v);
    }, []);
  return [value, flipValue];
}

/**
   Hook to re-render on Dome events (Custom React Hook).
   @param events - event names, defaults to a single `'dome.update'`.
*/
export function useUpdate(...events: Event<unknown>[]): void {
  const fn = useForceUpdate();
  React.useEffect(() => {
    const theEvents = events ? events.slice() : [update];
    theEvents.forEach((evt) => evt.on(fn));
    return () => theEvents.forEach((evt) => evt.off(fn));
  });
}

export interface PromiseHook<A> {
  result: A | undefined;
  error: Error | undefined;
  loading: boolean;
}

/**
   Hook to re-render when a Promise returns.
   The promise will be typically created by using `React.useMemo()`.
   The hook returns three information:
   - result: the promise result if it succeeds, undefined otherwise;
   - error: the promise error if it fails, undefined otherwise;
   - loading: the promise status, true if the promise is still running.
*/
export function usePromise<A>(job: Promise<A>): PromiseHook<A> {
  const [result, setResult] = React.useState<A | undefined>();
  const [error, setError] = React.useState<Error | undefined>();
  const [loading, setLoading] = React.useState(true);
  React.useEffect(() => {
    let c = false;
    const set = (x?: A, e?: Error): void => { setResult(x); setError(e); };
    const doCancel = (): boolean => { if (!c) setLoading(false); return c; };
    const onResult = (x: A): void => { if (!doCancel()) set(x, undefined); };
    const onError = (e: Error): void => { if (!doCancel()) set(undefined, e); };
    job.then(onResult, onError);
    return () => { c = true; };
  }, [job]);
  return { result, error, loading };
}

/* Internal type alias */
export type Serialize<A> = (a: A) => string;

/**
   Hook to add a cache system to a function, allowing to reuse previous results.
   As the equality used in JS maps does not allow to effectively implement a
   cache for complex type, a serialization function can be procured.
   The hook returns the cached version of the function.
*/
export function useCache<K, V>(r: (k: K) => V, s?: Serialize<K>): (k: K) => V {
  const { current: cache } = React.useRef(new Map<string, V>());
  const serialize = React.useMemo(() => s ? s : (k: K) => `${k}`, [s]);
  const get = React.useCallback((k: K): V => {
    const id = serialize(k);
    if (cache.has(id))
      return cache.get(id) as V;
    const v = r(k);
    cache.set(id, v);
    return v;
  }, [cache, r, serialize]);
  return get;
}

// --------------------------------------------------------------------------
// --- Timer Hooks
// --------------------------------------------------------------------------

interface Clock {
  timer?: NodeJS.Timeout;
  pending: number; // Number of listeners
  time: number; // Elapsed time since first pending
  event: string; // Tic events
  period: number; // Period
}

// Collection of clocks indexed by period
const CLOCKS = new Map<number, Clock>();

const CLOCKEVENT = (period: number): string => `dome.clock.${period}`;

const TIC_CLOCK = (clk: Clock) => (): void => {
  if (0 < clk.pending) {
    clk.time += clk.period;
    System.emitter.emit(clk.event, clk.time);
  } else {
    if (clk.timer) clearInterval(clk.timer);
    CLOCKS.delete(clk.period);
  }
};

const INC_CLOCK = (period: number): string => {
  let clk = CLOCKS.get(period);
  if (!clk) {
    const event = CLOCKEVENT(period);
    const time = (new Date()).getTime();
    clk = { pending: 0, time, period, event };
    clk.timer = setInterval(TIC_CLOCK(clk), period);
    CLOCKS.set(period, clk);
  }
  clk.pending++;
  return clk.event;
};

const DEC_CLOCK = (period: number): void => {
  const clk = CLOCKS.get(period);
  if (clk) clk.pending--;
};

export interface Timer {
  /** Starts or re-start the timer. */
  start(): void;
  /** Stops the timer. Can be restarted after. */
  stop(): void;
  /** Stops and reset elapsed time. */
  clear(): void;
  /** Running timer. */
  running: boolean;
  /** Elapsed time (in milliseconds). */
  time: number;
  /** Number of periods (rounded). */
  periods: number;
  /** Blink state (odd number of periods). */
  blink: boolean;
}

/**
   Synchronized start & stop timer (Custom React Hook).

   Create a local timer, synchronized on a global clock, that can be started
   and stopped on demand during the life cycle of the component.

   Each timer has its individual start & stop state. However,
   all timers with the same period _are_ synchronized with each others.

   @param period - timer interval, in milliseconds (ms)
   @param initStart - whether to initially start the timer (default is `false`)

 */
export function useClock(period: number, initStart = false): Timer {
  const started = React.useRef(0);
  const [time, setTime] = React.useState(0);
  const [running, setRunning] = React.useState(initStart);
  const start = React.useCallback(() => {
    setRunning(true);
    setTime(0);
  }, []);
  const stop = React.useCallback(() => {
    setRunning(false);
    setTime(0);
    started.current = 0;
  }, []);
  const clear = React.useCallback(() => {
    setRunning(false);
    started.current = time;
    setTime(0);
  }, [time]);
  React.useEffect(() => {
    if (!running) return undefined;
    const event = INC_CLOCK(period);
    const callback = (t: number): void => {
      if (!started.current) started.current = t;
      else setTime(t - started.current);
    };
    System.emitter.on(event, callback);
    return () => {
      System.emitter.off(event, callback);
      DEC_CLOCK(period);
    };
  }, [period, running]);
  const periods = Math.ceil(time / period);
  const blink = !!(periods & 1);
  return { running, time, periods, blink, start, stop, clear };
}

/**
   Register a polling callback on the given period.
   The polling is synchronized with all clocks and timers
   using the same period.
 */
export function useTimer(period: number, callback: () => void): void {
  React.useEffect(() => {
    const event = INC_CLOCK(period);
    System.emitter.on(event, callback);
    return () => {
      System.emitter.off(event, callback);
      DEC_CLOCK(period);
    };
  }, [period, callback]);
}

export type Callback<A> = (arg: A) => void;

/**
 * Protected callback against unwanted dependencies and unmounted component.
 * - The provided callback need _not_ be memoized, no dependency is needed.
 * - The provided callback will _not_ be fired after the component is unmounted.
 * - The returned callback will be _constant_ during the entire hook lifetime.
 */
export function useProtected<A>(fn: Callback<A> | undefined): Callback<A> {
  const cb = React.useRef<Callback<A>>();
  React.useEffect(() => {
    cb.current = fn;
    return () => { cb.current = undefined; };
  }, [fn]);
  const trigger = React.useCallback((arg: A) => {
    const fn = cb.current;
    if (fn) fn(arg);
  }, []);
  return trigger;
}

/**
 * A hook to retrieve text from a file.
 * The update is protected by the useProtected() hook.
 */
export function useFileContent(
  path: string,
): string {
  const [ fileContent, setFileContent ] = React.useState("");
  React.useEffect(() => {
    let alive = true;
    System.readFile(path)
      .then((response) => {
        if(alive) setFileContent(response.toString());
      })
      .catch((error) => {
      // eslint-disable-next-line no-console
      console.error('Error while loading the file :', error);
    });
    return () => { alive = false; };
  }, [path]);
  return fileContent;
}

/**
   Debounced callback (waiting time in milliseconds).
   The returned callback will be only fired when the component is mounted.
   The provided callback need not be memoized.
 */
export function useDebounced<A = void>(
  fn: Callback<A> | undefined,
  delay: number
): Callback<A> {
  const cb = React.useRef<Callback<A>>();
  React.useEffect(() => {
    cb.current = fn;
    return () => { cb.current = undefined; };
  }, [fn]);
  const trigger = React.useMemo(
    () => _.debounce((arg: A) => {
      const fn = cb.current;
      if (fn) fn(arg);
    }, delay), [delay]);
  return trigger;
}

/**
   Throttled callback (waiting time in milliseconds).
   The returned callback will be only fired when the component is mounted.
   The provided callback need not be memoized.
 */
export function useThrottled<A = void>(
  fn: Callback<A> | undefined,
  period: number
): Callback<A> {
  const cb = React.useRef<Callback<A>>();
  React.useEffect(() => {
    cb.current = fn;
    return () => { cb.current = undefined; };
  }, [fn]);
  const trigger = React.useMemo(
    () => _.throttle((arg: A) => {
      const fn = cb.current;
      if (fn) fn(arg);
    }, period), [period]);
  return trigger;
}

// --------------------------------------------------------------------------
// --- Sampling Hookds
// --------------------------------------------------------------------------

export type range = [number, number];
const NORANGE: range = [0, 0];

/**
   Static sampler. Accumulates instant values and compute their mean,
   min and max values.
 */
export class Sampler {
  private samples: number[];
  private range: [number, number] | undefined;
  private total = 0;
  private index = 0;
  private values = 0;
  private current = 0;

  /** @param n - maximum number of sampled values */
  constructor(n: number) {
    this.samples = new Array(n).fill(0);
  }

  /** Resets the sampler. Forgets all previous measures. */
  reset(): void {
    this.index = 0;
    this.values = 0;
    this.current = 0;
    this.total = 0;
    this.range = undefined;
    this.samples.fill(0);
  }

  /** Set the current instant value. */
  setValue(m: number): void { this.current = m; }

  /** Add or remove the given amount to the current instant value. */
  addValue(d: number): void { this.current += d; }

  /**
     Register the given instant value `v` to the sampler.
     This is a shortcut to `setValue(v)` followed by `flush()`.
   */
  pushValue(v: number): void {
    this.current = v;
    this.flush();
  }

  /**
     Register the current instant value to the sampler.
     The current instant value is left unchanged.
   */
  flush(): void {
    const v = this.current;
    const n = this.values;
    const size = this.samples.length;
    const rg = this.range;
    if (rg) {
      const [a, b] = rg;
      if (v < a || b < v) this.range = undefined;
    }
    if (n < size) {
      this.samples[n] = v;
      this.total += v;
      this.values++;
    } else {
      const k = this.index;
      if (k + 1 < size) {
        const v0 = this.samples[k];
        this.samples[k] = v;
        this.total += v - v0;
        this.index++;
      } else {
        this.samples[k] = v;
        this.index = 0;
        this.total = this.samples.reduce((s, x) => s + x, 0);
      }
    }
  }

  /**
     Returns the sum of all sampled values.
     In case the sampler is empty, returns `0`.
   */
  getTotal(): number { return this.total; }

  /**
     Returns the mean of sampled values.
     In case the sampler is empty, returns `undefined`.
   */
  getMean(): number | undefined {
    const n = this.values;
    return n > 0 ? this.total / n : undefined;
  }

  /**
     Returns the `[min,max]` range of sampled values.
     In case the sampler is empty, returns `[0,0]`.
   */
  getRange(): range {
    const rg = this.range;
    if (rg !== undefined) return rg;
    const n = this.values;
    if (n <= 0) return NORANGE;
    let a = +Infinity;
    let b = -Infinity;
    for (let k = 0; k < n; k++) {
      const s = this.samples[k];
      if (s < a) a = s;
      if (s > b) b = s;
    }
    const newrg: range = [a, b];
    this.range = newrg;
    return newrg;
  }

}

export interface Sample {
  max: number;
  min: number;
  value: number | undefined;
}

/**
   Hook to periodically listen on a global sampler.
 */
export function useSampler(S: Sampler, polling: number): Sample {
  const [sample, setSample] = React.useState<Sample>(
    { min: 0, value: undefined, max: 0 }
  );
  useTimer(polling, () => {
    const m = S.getMean();
    const [a, b] = S.getRange();
    if (m !== sample.value || a !== sample.min || b !== sample.max)
      setSample({ value: m, min: a, max: b });
  });
  return sample;
}

// --------------------------------------------------------------------------
// --- Settings Hookds
// --------------------------------------------------------------------------

/**
   Bool window settings helper. Default is `false` unless specified.
 */
export function useBoolSettings(
  key: string | undefined,
  defaultValue = false,
): State<boolean> {
  return Settings.useWindowSettings(
    key, Json.jBoolean, defaultValue,
  );
}

/**
   Bool window settings helper with a flip callback.
   See also {useFlipState}.
 */
export function useFlipSettings(
  key: string | undefined,
  defaultValue = false,
): [boolean, () => void] {
  const [state, setState] = Settings.useWindowSettings(
    key, Json.jBoolean, defaultValue,
  );
  return [state, () => setState(!state)];
}

/** Number window settings helper. Default is `0` unless specified. */
export function useNumberSettings(
  key: string | undefined,
  defaultValue = 0,
): State<number> {
  return Settings.useWindowSettings(
    key, Json.jNumber, defaultValue,
  );
}

/** String window settings. Default is `''` unless specified). */
export function useStringSettings(
  key: string | undefined,
  defaultValue = ''
): State<string> {
  return Settings.useWindowSettings(
    key, Json.jString, defaultValue,
  );
}

/** Optional string window settings. Default is `undefined`. */
export function useStringOptSettings(
  key: string | undefined
): State<string | undefined> {
  return Settings.useWindowSettings(
    key, Json.jString, undefined,
  );
}

/** Direct shortcut to [[dome/data/settings.useWindowSettings]]. */
export const { useWindowSettings } = Settings;

/**
   Utility shortcut to [[dome/data/settings.useGlobalSettings]]
   with global settings class created on-the-fly.
 */
export function useGlobalSettings<A extends Json.json>(
  globalKey: string,
  decoder: Json.Decoder<A>,
  defaultValue: A,
): State<A> {
  // Object creation is cheaper than useMemo...
  const G = new Settings.GlobalSettings(
    globalKey, decoder, Json.identity, defaultValue,
  );
  return Settings.useGlobalSettings(G);
}


// --------------------------------------------------------------------------
// --- Path resources
// --------------------------------------------------------------------------

export function getResourcesPath(): string {
  const prodPath = !DEVEL && __dirname.includes("out/renderer") ?
    __dirname : process.resourcesPath;
  return DEVEL ? `/` : `file://${prodPath}`;
}

export function getResourcePath(filePath: string): string {
  const resourcesPath = getResourcesPath();
  return path.join(resourcesPath, filePath);
}

// --------------------------------------------------------------------------
