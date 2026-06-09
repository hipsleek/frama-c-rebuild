/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Connection to Frama-C Server
// --------------------------------------------------------------------------

/**
 Manage the current Frama-C server/client interface
 @packageDocumentation
 @module frama-c/server
*/

import { debounce } from 'lodash';
import * as Path from 'path';
import React from 'react';
import * as Dome from 'dome';
import * as System from 'dome/misc/system';
import * as Json from 'dome/data/json';
import { TextBuffer } from 'dome/text/richtext';
import { ChildProcess } from 'child_process';
import { client } from './client_socket';
import { logModel, status as statutLog } from './kernel/ServerLogs';
// import { client } from './client_zmq';

// --------------------------------------------------------------------------
// --- Server Status
// --------------------------------------------------------------------------

/** Server stages. */
export enum Status {
  /** Server is off. */
  OFF = 'OFF',
  /** Server is starting, but not on yet. */
  STARTING = 'STARTING',
  /** Server is running. */
  ON = 'ON',
  /** Server is running command line. */
  CMD = 'CMD',
  /** Server is halting, but not off yet. */
  HALTING = 'HALTING',
  /** Server is restarting. */
  RESTARTING = 'RESTARTING',
  /** Server is off upon failure. */
  FAILURE = 'FAILURE',
}

const unreachable = (_c: never): never => { throw ('unreachable'); };

// --------------------------------------------------------------------------
// --- Events
// --------------------------------------------------------------------------

/**
 *  Server Status Notification Event.

 *  This event is emitted whenever the server status changes.
 */
const STATUS = new Dome.Event<Status>('frama-c.server.status');

/**
 *  Server is actually started and running.

 *  This event is emitted when the server _enters_ the `ON` or `CMD` state.
 *  The server is now ready to handle requests.
 */
const READY = new Dome.Event('frama-c.server.ready');

/**
 *  Server Status Notification Event

 *  This event is emitted when the server _leaves_ the `ON` or `CMD` state.
 *  The server is no more able to handle requests until restart.
 */
const SHUTDOWN = new Dome.Event('frama-c.server.shutdown');

/**
 *  Server Signal event constructor.

 *  Event `frama-c.server.signal.<id>'` for signal `<id>`.
 */
export class SIGNAL extends Dome.Event {
  constructor(signal: string) {
    super(`frama-c.server.signal.${signal}`);
  }
}

// --------------------------------------------------------------------------
// --- Server Global State
// --------------------------------------------------------------------------

/** The current server status. */
let status: Status = Status.OFF;

/** Request counter. */
let rqCount = 0;

/** Pending Requests. */
interface PendingRequest {
  resolve: (data: Json.json) => void;
  reject: (err?: string) => void;
}
const pending = new Map<string, PendingRequest>();

/** Server process. */
let process: ChildProcess | undefined;

/** Polling timeout when server is busy.
    Note: Inet sockets are more prone to timeouts than Unix sockets. */
const pollingTimeout = 200;
let pollingTimer: NodeJS.Timeout | undefined;

/** Killing timeout and timer for server process hard kill. */
const killingTimeout = 1000;
let killingTimer: NodeJS.Timeout | undefined;

// --------------------------------------------------------------------------
// --- Server Console
// --------------------------------------------------------------------------

/** The server console buffer. */
export const buffer = new TextBuffer();

// --------------------------------------------------------------------------
// --- Server Status
// --------------------------------------------------------------------------

/**
 *  Current server status.
 *  @return {Status} The current server status.
 */
export function getStatus(): Status { return status; }

/**
 *  Hook on current server (Custom React Hook).
 *  @return {Status} The current server status.
 */
export function useStatus(): Status {
  Dome.useUpdate(STATUS);
  return status;
}

export const isRunningStatus = (st: Status): boolean =>
  (st === Status.ON || st === Status.CMD);

/**
 *  Whether the server is running and ready to handle requests.
 *  @return {boolean} Whether server is in running stage,
 *  defined by status `ON` or `CMD`.
 */
export function isRunning(): boolean {
  return isRunningStatus(status);
}

/**
 *  Number of requests still pending.
 *  @return {number} Pending requests.
 */
export function getPending(): number {
  return pending.size;
}

/**
 *  Register callback on `READY` event.
 *  @param {function} callback Invoked when the server enters running stage.
 */
export function onReady(callback: () => void): void {
  READY.on(callback);
}

/**
 *  Register callback on `SHUTDOWN` event.
 *  @param {function} callback Invoked when the server leaves running stage.
 */
export function onShutdown(callback: () => void): void {
  SHUTDOWN.on(callback);
}

/**
 *  Register callback on server status events.
 *  @param {function} callback Invoked when the server leaves running stage.
 */
export function onStatus(callback: (s: Status) => void): void {
  STATUS.on(callback);
}

// --------------------------------------------------------------------------
// --- Status Hooks
// --------------------------------------------------------------------------

export function useReady(callback: () => void): void {
  React.useEffect(() => {
    READY.on(callback);
    return () => READY.off(callback);
  }, [callback]);
}

export function useShutdown(callback: () => void): void {
  React.useEffect(() => {
    SHUTDOWN.on(callback);
    return () => SHUTDOWN.off(callback);
  }, [callback]);
}

// --------------------------------------------------------------------------
// --- Status Update
// --------------------------------------------------------------------------

function _status(newStatus: Status): void {
  if (newStatus !== status) {
    const oldStatus = status;
    status = newStatus;
    STATUS.emit(newStatus);
    const oldRun = isRunningStatus(oldStatus);
    const newRun = isRunningStatus(newStatus);
    if (oldRun && !newRun) SHUTDOWN.emit();
    if (!oldRun && newRun) READY.emit();
  }
}

const _update: () => void = debounce(() => STATUS.emit(status), 100);

// --------------------------------------------------------------------------
// --- Server Control (Start)
// --------------------------------------------------------------------------

/**
 *  Start the server.
 *
 *  - If the server is either started or running, this is a no-op.
 *  - If the server is halting, it will restart.
 *  - Otherwise, the Frama-C server is spawned.
 */
export async function start(): Promise<void> {
  switch (status) {
    case Status.OFF:
    case Status.FAILURE:
    case Status.RESTARTING:
      logModel.clear();
      logModel.registerFeedback('Server starting...');
      _status(Status.STARTING);
      try {
        await _launch();
      } catch (error) {
        buffer.append('[frama-c]', error, '\n');
        _exit(false);
      }
      return;
    case Status.HALTING:
      logModel.clear();
      logModel.registerFeedback('Server restarting...');
      _status(Status.RESTARTING);
      return;
    case Status.ON:
    case Status.CMD:
    case Status.STARTING:
      return;
    default:
      unreachable(status);
  }
}

// --------------------------------------------------------------------------
// --- Server Control (Stop)
// --------------------------------------------------------------------------

/**
 *  Stop the server.
 *
 *  - If the server is starting, it is hard killed.
 *  - If the server is running, it is shutdown gracefully.
 *  - If the server is halting, it is hard killed.
 *  - Otherwise, this is a no-op.
 */
export function stop(): void {
  switch (status) {
    case Status.STARTING:
      _status(Status.HALTING);
      _kill();
      return;
    case Status.ON:
    case Status.CMD:
    case Status.HALTING:
      _status(Status.HALTING);
      _shutdown();
      return;
    case Status.RESTARTING:
      _status(Status.HALTING);
      return;
    case Status.OFF:
      return;
    case Status.FAILURE:
      _status(Status.OFF);
      return;
    default:
      unreachable(status);
  }
}

Dome.atExit(stop);

// --------------------------------------------------------------------------
// --- Server Control (Kill)
// --------------------------------------------------------------------------

/**
 *  Terminate the server.
 *
 *  - If the server is either starting, running or shutting down,
 *    it is hard killed.
 *  - Otherwise, this is a no-op.
 */
export function kill(): void {
  switch (status) {
    case Status.ON:
    case Status.CMD:
    case Status.HALTING:
    case Status.STARTING:
    case Status.RESTARTING:
      logModel.registerFeedback('Killing server...');
      _status(Status.HALTING);
      _kill();
      return;
    case Status.OFF:
      return;
    case Status.FAILURE:
      _status(Status.OFF);
      return;
    default:
      unreachable(status);
  }
}

// --------------------------------------------------------------------------
// --- Server Control (Restart)
// --------------------------------------------------------------------------

/**
 *  Restart the server.
 *
 *  - If the server is either off or paused on failure, simply start the server.
 *  - If the server is running, try to gracefully shutdown the server,
 *  and finally schedule a reboot on exit.
 *  - Otherwise, this is a no-op.
 */
export function restart(): void {
  switch (status) {
    case Status.OFF:
    case Status.FAILURE:
      start();
      return;
    case Status.ON:
    case Status.CMD:
      logModel.clear();
      logModel.registerFeedback('Server restarting...');
      _status(Status.RESTARTING);
      _shutdown();
      return;
    case Status.HALTING:
      logModel.clear();
      logModel.registerFeedback('Server restarting...');
      _status(Status.RESTARTING);
      return;
    case Status.STARTING:
    case Status.RESTARTING:
      return;
    default:
      unreachable(status);
  }
}

// --------------------------------------------------------------------------
// --- Server Control (Reset)
// --------------------------------------------------------------------------

/**
 *  Acknowledge the [[OFF]] or [[FAILURE]] stages.
 *
 *  - If the server is either off or paused on failure,
 *  clear the console and set server stage to [[OFF]].
 *  - Otherwise, this is a no-op.
 */
export function clear(): void {
  switch (status) {
    case Status.OFF:
    case Status.FAILURE:
      buffer.clear();
      _clear();
      _status(Status.OFF);
      return;
  }
}

// --------------------------------------------------------------------------
// --- Server Configure
// --------------------------------------------------------------------------

export const defaultInetAddress = '127.0.0.1';
export const defaultInetPort = 9225;
export type SocketDomain = 'internet' | 'unix';

/** Server configuration. */
export interface Configuration {
  /** Process environment variables (default: `undefined`). */
  env?: { [VAR: string]: string };
  /** Working directory (default: current). */
  working?: string;
  /** Server command (default: `frama-c`). */
  command?: string;
  /** Additional server arguments (default: empty). */
  params: string[];
  /** Server socket domain (default: `unix`, except on Windows, where it
      is `internet`). */
  sockdomain?: SocketDomain;
  /** Server socket (default: `ipc:///tmp/ivette.frama-c.<pid>.io` for Unix
      sockets, or `127.0.0.1:9225` for Inet sockets). */
  sockaddr?: string;
  /** Shutdown timeout before server is hard killed, in milliseconds
   *  (default: 1000ms). */
  timeout?: number;
  /** Server polling period in milliseconds (default: 200ms). */
  polling?: number;
  /** Process stdout log file (default: `undefined`). */
  logout?: string;
  /** Process stderr log file (default: `undefined`). */
  logerr?: string;
}

/** Server current configuration. */
let config: Configuration = { command: 'frama-c', params: [] };

/**
 *  Set the current server configuration.
 *  @param {Configuration} sc Server configuration.
 */
export function setConfig(sc: Configuration): void {
  config = { ...sc };
}

/**
 *  Get the current server configuration.
 *  @return {Configuration} Current server configuration.
 */
export function getConfig(): Configuration {
  return config;
}

/**
   Resolve the file-path with respect to current server config.
 */

export function getPath(path: string): string {
  const cwd = config.working ?? System.getWorkingDir();
  return Path.resolve(cwd, path);
}

// --------------------------------------------------------------------------
// --- Low-level Launching
// --------------------------------------------------------------------------

function rsplit(s: string, delim: string): string[] {
  const i = s.lastIndexOf(delim);
  return i === -1 ? [s] : [s.slice(0, i), s.slice(i+1)];
}

async function _launch(): Promise<void> {
  let {
    env,
    working,
    command = 'frama-c',
    params,
    sockdomain,
    sockaddr,
    logout,
    logerr,
  } = config;

  if (!sockdomain) {
    /* Note: we were unable to get Unix sockets working on Windows,
       despite the fact that it should be possible.
       For now, Windows uses Inet sockets by default. */
    sockdomain = System.platform === 'windows' ? 'internet' : 'unix';
  } else {
    // ensure that sockdomain has a valid value
    if (sockdomain !== 'internet' && sockdomain !== 'unix') {
      throw new Error("expected 'unix' or 'internet'");
    }
  }

  if (!sockaddr) {
    switch (sockdomain) {
      case 'internet':
        sockaddr = '127.0.0.1:9225';
        break;
      case 'unix': {
        const tmp = Dome.getTempDir();
        const pid = Dome.getPID();
        sockaddr = System.join(tmp, `ivette.frama-c.${pid}.io`);
      }
    }
  }

  const prelude = window.electron.process.env.FRAMAC_SERVER_PRELUDE ?
    window.electron.process.env.FRAMAC_SERVER_PRELUDE.split(" ") : [];
  params = client.commandLine(sockdomain, sockaddr, params, prelude);

  buffer.clear();
  buffer.append('$', command);
  const size = params.reduce((n, p) => n + p.length, 0);
  if (size < 40) {
    buffer.append('', ...params);
  } else {
    params.forEach((argv: string) => {
      if (argv.startsWith('-') || argv.endsWith('.c')
        || argv.endsWith('.i') || argv.endsWith('.h')) {
        buffer.append('\n    ');
      }
      buffer.append(' ');
      buffer.append(argv);
    });
  }
  buffer.append('\n');

  if (!working) working = System.getWorkingDir();
  logout = logout && System.join(working, logout);
  logerr = logerr && System.join(working, logerr);
  const options = {
    cwd: working,
    stdout: { path: logout, pipe: true },
    stderr: { path: logerr, pipe: true },
    env: {
      ...env,
      ...window.electron.process.env
    } as { [VAR: string]: string },
  };
  // Launch Process
  System.atExit(() => {
    sockaddr && System.remove(sockaddr);
  });
  process = await System.spawn(command, params, options);
  process?.stdout?.on('data', buffer.append);
  process?.stderr?.on('data', buffer.append);
  process?.on('exit', (code: number | null, signal: string | null) => {
    if (signal !== null) {
      buffer.append('[frama-c]', signal, '\n');
      _exit(false);
      return;
    }
    if (code !== 0) {
      buffer.append('[frama-c] exit', code, '\n');
      _exit(true);
      return;
    }
    if (Dome.DEVEL)
      buffer.append('[frama-c] terminated.\n');
    _exit(false);
    return;
  });
  logModel.registerFeedback('Socket server is running');
  // Connect to Server
  switch (sockdomain) {
    case 'internet': {
      // For TCP, Net.createConnection's parameters are port, then host
      let host, port, portStr, inetSockaddr : [string, number];
      if (sockaddr.includes(":")) {
        [host, portStr] = rsplit(sockaddr, ":");
        port = parseInt(portStr);
        inetSockaddr = [host, port];
      } else {
        inetSockaddr = [sockaddr, defaultInetPort];
      }
      client.connect(inetSockaddr);
      break;
    }
    case 'unix':
      client.connect(sockaddr);
  }
}

// --------------------------------------------------------------------------
// --- Polling Management
// --------------------------------------------------------------------------

function _startPolling(): void {
  if (!pollingTimer) {
    const polling = (config && config.polling) || pollingTimeout;
    pollingTimer = setInterval(() => {
      client.poll();
    }, polling);
  }
}

function _stopPolling(): void {
  if (pollingTimer) {
    clearInterval(pollingTimer);
    pollingTimer = undefined;
  }
}

// --------------------------------------------------------------------------
// --- Low-level Killing
// --------------------------------------------------------------------------

function _clear(): void {
  rqCount = 0;
  pending.forEach((p: PendingRequest) => p.reject('clear'));
  pending.clear();
  _stopPolling();
  if (killingTimer) {
    clearTimeout(killingTimer);
    killingTimer = undefined;
  }
}

function _kill(): void {
  client.disconnect();
  if (process) process.kill();
}

async function _shutdown(): Promise<void> {
  _clear();
  client.shutdown();
  logModel.registerFeedback('Server shutdown');
  const killingPromise = new Promise((resolve) => {
    if (!killingTimer) {
      if (process) {
        const timeout = (config && config.timeout) || killingTimeout;
        killingTimer =
          setTimeout(() => {
            resolve(process?.kill());
          }, timeout);
      }
    }
  });
  await killingPromise;
}

function _exit(error: boolean): void {
  _clear();
  client.disconnect();
  process = undefined;
  if (status === Status.RESTARTING) {
    setImmediate(start);
  }
  _status(error ? Status.FAILURE : Status.OFF);
}

// --------------------------------------------------------------------------
// --- Signal Management
// --------------------------------------------------------------------------

class SignalHandler {
  id: string;
  event: Dome.Event;
  active: boolean;
  listen: boolean;
  handler: _.DebouncedFunc<() => void>;

  constructor(id: string) {
    this.id = id;
    this.event = new SIGNAL(id);
    this.active = false;
    this.listen = false;
    this.sigon = this.sigon.bind(this);
    this.sigoff = this.handler = debounce(this.sigoff.bind(this), 1000);
    this.unplug = this.unplug.bind(this);
  }

  on(callback: () => void): void {
    const e = this.event;

    const n = e.listenerCount();
    e.on(callback);
    if (n === 0) {
      this.active = true;
      if (isRunning()) this.sigon();
    }
  }

  off(callback: () => void): void {
    const e = this.event;
    e.off(callback);
    const n = e.listenerCount();
    if (n === 0) {
      this.active = false;
      if (isRunning()) this.sigoff();
    }
  }

  /* Bound to this */
  sigon(): void {
    if (this.active && !this.listen) {
      this.listen = true;
      client.sigOn(this.id);
      logModel.registerSignal(this.id, statutLog.SIGON);
    }
  }

  /* Bound to this, Debounced */
  sigoff(): void {
    if (!this.active && this.listen) {
      if (isRunning()) {
        this.listen = false;
        client.sigOff(this.id);
        logModel.registerSignal(this.id, statutLog.SIGOFF);
      }
    }
  }

  emit(): void {
    this.event.emit();
  }

  unplug(): void {
    this.listen = false;
    this.handler.cancel();
  }
}

// --- Memo

const signals: Map<string, SignalHandler> = new Map();

function _signal(id: string): SignalHandler {
  let s = signals.get(id);
  if (!s) {
    s = new SignalHandler(id);
    signals.set(id, s);
  }
  return s;
}

// --- External API

/**
 *  Register a callback for a signal.
 *
 *  If the server is not yet listening to this signal, a `SIGON` command is
 *  sent to the Frama-C server.
 *  @param {string} id The signal identifier to listen to.
 *  @param {function} callback The callback to call upon signal.
 */
export function onSignal(id: Signal, callback: () => void): void {
  _signal(id.name).on(callback);
}

/**
 *  Unregister a callback of a signal.
 *
 *  When no more callbacks are listening to this signal for a while,
 *  the Frama-C server will be notified with a `SIGOFF` command.
 *  @param {string} id The signal identifier that was listen to.
 *  @param {function} callback The callback to remove.
 */
export function offSignal(id: Signal, callback: () => void): void {
  _signal(id.name).off(callback);
}

/**
 *  Hook on a signal (Custom React Hook).
 *  @param {string} id The signal identifier to listen to.
 *  @param {function} callback The callback to call upon signal.
 */
export function useSignal(id: Signal, callback: () => void): void {
  React.useEffect(() => {
    onSignal(id, callback);
    return () => { offSignal(id, callback); };
  });
}

// --- Server Synchro

READY.on(() => {
  signals.forEach((h: SignalHandler) => {
    h.sigon();
  });
});

SHUTDOWN.on(() => {
  signals.forEach((h: SignalHandler) => {
    h.unplug();
  });
});

// --------------------------------------------------------------------------
// --- REQUEST Management
// --------------------------------------------------------------------------

/** Request kind. */
export enum RqKind {
  /** Used to read data from the Frama-C server. */
  GET = 'GET',
  /** Used to write data into the Frama-C server. */
  SET = 'SET',
  /** Used to make the Frama-C server execute a task. */
  EXEC = 'EXEC',
}

/** Server signal. */
export interface Signal {
  name: string;
}

/** Server request. */
export interface Request<Kd extends RqKind, In, Out> {
  kind: Kd;
  /** The request full name. */
  name: string;
  /** Encoder of input parameters. */
  input: Json.Decoder<In>;
  /** Decoder of output parameters. */
  output: Json.Decoder<Out>;
  /** Fallback output value */
  fallback: Out;
  /** Signals the request depends on */
  signals: Array<Signal>;
}

export type GetRequest<In, Out> = Request<RqKind.GET, In, Out>;
export type SetRequest<In, Out> = Request<RqKind.SET, In, Out>;
export type ExecRequest<In, Out> = Request<RqKind.EXEC, In, Out>;

export interface ServerTask<Data> extends Promise<Data> {
  kill?: () => void;
}

/**
 *  Send a request to the server.
 *
 *  You may _kill_ the request before its normal termination by
 *  invoking `kill()` on the returned promised.
 */
export function send<In, Out>(
  request: Request<RqKind, In, Out>,
  param: In,
): ServerTask<Out> {
  if (!isRunning()) return Promise.reject('Server not running');
  if (!request.name) return Promise.reject('Undefined request');
  const rid = `RQ.${rqCount}`;
  rqCount += 1;
  const { kind, name } = request;
  logModel.registerRequest({
    rid, kind, name,
    param: param as unknown as Json.json,
    statut: statutLog.PENDING
  });
  const response: ServerTask<Out> = new Promise<Out>((resolve, reject) => {
    const unwrap = (js: Json.json): void => {
      try {
        resolve(request.output(js));
      } catch (err) {
        const message = `Invalid ${request.name} response: ${err}`;
        if (err instanceof Json.JsonError) {
          reject(`${message}\nThe whole response was:\n${Json.stringify(js)}`);
        }
        else {
          reject(message);
        }
      }
    };
    pending.set(rid, { resolve: unwrap, reject });
  });
  response.kill = () => pending.get(rid)?.reject('kill');
  client.send(request.kind, rid, request.name, param as unknown as Json.json);
  _startPolling();
  _update();
  return response;
}

// --------------------------------------------------------------------------
// --- Client Events
// --------------------------------------------------------------------------

function _resolved(id: string): void {
  pending.delete(id);
  if (pending.size === 0 && status === Status.ON) {
    rqCount = 0;
    _stopPolling();
    _update();
  }
}

client.onConnect((err?: Error) => {
  if (err) {
    _status(Status.FAILURE);
    _clear();
  } else {
    logModel.registerFeedback('Client connected');
    _status(Status.CMD);
    _startPolling();
  }
});

client.onData((id: string, data: Json.json) => {
  const p = pending.get(id);
  if (p) {
    p.resolve(data);
    _resolved(id);
    logModel.registerRequest(
      { rid: id, param: data, statut: statutLog.RESOLVED }
    );
  }
});

client.onKilled((id: string) => {
  const p = pending.get(id);
  if (p) {
    p.reject('killed');
    _resolved(id);
  }
});

client.onRejected((id: string) => {
  const p = pending.get(id);
  if (p) {
    const err =
     'Request rejected by Frama-C: the related plugin may not be loaded, ' +
     'or the Frama-C version is incompatible.';
    p.reject(err);
    _resolved(id);
    logModel.registerRequest(
      { rid: id, statut: statutLog.REJECTED }
    );
  }
});

client.onError((id: string, msg: string) => {
  const p = pending.get(id);
  if (p) {
    p.reject(`error (${msg})`);
    _resolved(id);
    logModel.registerRequest(
      { rid: id, param: msg, statut: statutLog.ERROR }
    );
  }
});

client.onSignal((id: string) => {
  _signal(id).emit();
});

client.onCmdLine((cmd: boolean) => {
  _status(cmd ? Status.CMD : Status.ON);
  if (cmd)
    _startPolling();
  else {
    if (pending.size === 0)
      _stopPolling();
  }
});

// --------------------------------------------------------------------------
