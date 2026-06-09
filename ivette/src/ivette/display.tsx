/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Display Interaction                                                --- */
/* -------------------------------------------------------------------------- */

/**
   @packageDocumentation
   @module ivette/display
 */

import { LayoutPosition } from 'ivette';
import * as Laboratory from './laboratory';

/** Switch display to specified view. */
export function switchToView(id: string): void {
  Laboratory.switchToView(id);
}

/** Show component. */
export function showComponent(id: string, at?: LayoutPosition): void {
  Laboratory.showComponent(id, at);
}

/** Dock component. */
export function dockComponent(id: string, at?: LayoutPosition): void {
  Laboratory.dockComponent(id, at);
}

/** Alert component. */
export function alertComponent(id: string): void {
  Laboratory.alertComponent(id);
}

/** Component Status Hook. */
export function useComponentStatus(
  id: string | undefined
): Laboratory.ComponentStatus {
  const state = Laboratory.useState();
  return Laboratory.getComponentStatus(state, id ?? '');
}

export type Message = string | { label: string, title: string };

/** Message notification */
export function showMessage(msg: Message): void {
  if (!msg) return;
  const short = typeof msg === 'string';
  const label = short ? msg : msg.label;
  const title = short ? msg : msg.title;
  Laboratory.showMessage({ kind: "message", label, title });
}

/** Warning notification. */
export function showWarning(msg: Message): void {
  if (!msg) return;
  const short = typeof msg === 'string';
  const label = short ? msg : msg.label;
  const title = short ? msg : msg.title;
  Laboratory.showMessage({ kind: 'warning', label, title });
}

/** Error notification */
export function showError(msg: Message): void {
  if (!msg) return;
  const short = typeof msg === 'string';
  const label = short ? msg : msg.label;
  const title = short ? msg : msg.title;
  Laboratory.showMessage({ kind: 'error', label, title });
}

export function clearMessages(): void {
  Laboratory.clearMessages();
}

/* -------------------------------------------------------------------------- */
