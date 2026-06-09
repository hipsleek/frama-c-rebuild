/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import Emitter from 'events';
import { json } from 'dome/data/json';

// --------------------------------------------------------------------------
// --- Frama-C Server Access (Client side)
// --------------------------------------------------------------------------

export abstract class Client {

  /** Server CLI */
  abstract commandLine(domain: string, sockaddr: string, params: string[],
                       prelude: string[]):
  string[];

  /** Connection */
  abstract connect(addr: string | [string, number]): void;

  /** Disconnection */
  abstract disconnect(): void;

  /** Send Request */
  abstract send(kind: string, id: string, request: string, data: json): void;

  /** Signal ON */
  abstract sigOn(id: string): void;

  /** Signal OFF */
  abstract sigOff(id: string): void;

  /** Kill Request */
  abstract kill(id: string): void;

  /** Polling */
  abstract poll(): void;

  /** Shutdown the server */
  abstract shutdown(): void;

  /** @internal */
  private events = new Emitter();

  // --------------------------------------------------------------------------
  // --- DATA Event
  // --------------------------------------------------------------------------

  /** Request data callback */
  onData(callback: (id: string, data: json) => void): void {
    this.events.on('DATA', callback);
  }

  /** @internal */
  emitData(id: string, data: json): void {
    this.events.emit('DATA', id, data);
  }

  // --------------------------------------------------------------------------
  // --- REJECTED Event
  // --------------------------------------------------------------------------

  /** Rejected request callback */
  onRejected(callback: (id: string) => void): void {
    this.events.on('REJECTED', callback);
  }

  /** @internal */
  emitRejected(id: string): void {
    this.events.emit('REJECTED', id);
  }

  // --------------------------------------------------------------------------
  // --- ERROR Event
  // --------------------------------------------------------------------------

  /** Rejected request callback */
  onError(callback: (id: string, msg: string) => void): void {
    this.events.on('ERROR', callback);
  }

  /** @internal */
  emitError(id: string, msg: string): void {
    this.events.emit('ERROR', id, msg);
  }

  // --------------------------------------------------------------------------
  // --- KILLED Event
  // --------------------------------------------------------------------------

  /** Killed request callback */
  onKilled(callback: (id: string) => void): void {
    this.events.on('KILLED', callback);
  }

  /** @internal */
  emitKilled(id: string): void {
    this.events.emit('KILLED', id);
  }

  // --------------------------------------------------------------------------
  // --- SIGNAL Event
  // --------------------------------------------------------------------------

  /** Signal callback */
  onSignal(callback: (id: string) => void): void {
    this.events.on('SIGNAL', callback);
  }

  /** @internal */
  emitSignal(id: string): void {
    this.events.emit('SIGNAL', id);
  }

  // --------------------------------------------------------------------------
  // --- CONNECT Event
  // --------------------------------------------------------------------------

  /** Connection callback */
  onConnect(callback: (err?: Error) => void): void {
    this.events.on('CONNECT', callback);
  }

  /** @internal */
  emitConnect(err?: Error): void {
    this.events.emit('CONNECT', err);
  }

  // --------------------------------------------------------------------------
  // --- CMDLINE Event
  // --------------------------------------------------------------------------

  /** Signal callback */
  onCmdLine(callback: (cmd: boolean) => void): void {
    this.events.on('CMDLINE', callback);
  }

  /** @internal */
  emitCmdLine(cmd: boolean): void {
    this.events.emit('CMDLINE', cmd);
  }

}

// --------------------------------------------------------------------------
