/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Extensible Search Field
// --------------------------------------------------------------------------

import React from 'react';
import * as Dome from 'dome';
import * as Toolbar from 'dome/frame/toolbars';
import { GlobalState, useGlobalState } from 'dome/data/states';

// --------------------------------------------------------------------------
// --- Search Actions Registry
// --------------------------------------------------------------------------

export interface Hint {
  id: string;
  name?: string;
  icon?: string;
  label?: string;
  title?: string;
  rank?: number;
  onClick?: () => void;
}

export interface ModeProps {
  id: string;
  rank?: number;
  icon?: string;
  label?: string;
  title?: string;
  placeholder?: string;
  className?: string;
  enabled?: boolean;
  hints?: () => Hint[];
  onHint?: (hint: Hint) => void;
  onEnter?: (pattern: string) => void;
}

const defaultMode: ModeProps = { id: '' };

class ModeManager extends GlobalState<ModeProps> {
  constructor() { super(defaultMode); }
  private registry: Map<string, ModeProps> = new Map();

  find(id: string): ModeProps | undefined {
    return this.registry.get(id);
  }

  register(mode: ModeProps): void {
    const { id, icon = 'SEARCH', ...data } = mode;
    this.registry.set(id, { id, icon, ...data });
    const id0 = this.getValue().id;
    if (id0 === '' || id0 === id) this.setValue(mode);
  }

  update(m: ModeProps): void {
    const { id, ...data } = m;
    const curr = this.registry.get(id) ?? {};
    const mode = { ...curr, ...data, id };
    this.register(mode);
  }

  remove(id: string): void {
    this.registry.delete(id);
    const id0 = this.getValue().id;
    if (id0 === id) this.setValue(defaultMode);
  }

  selfhints(): Hint[] {
    return Array.from(this.registry.values());
  }
}

const allModes = new ModeManager();
const focus = new Dome.Event<void>('ivette.search.focus');

// --------------------------------------------------------------------------
// --- Ivette Extension Points
// --------------------------------------------------------------------------

export function registerMode(m: ModeProps): void { allModes.register(m); }
export function updateMode(m: ModeProps): void { allModes.update(m); }
export function removeMode(id: string): void { allModes.remove(id); }

export function findMode(id: string): ModeProps | undefined {
  return allModes.find(id);
}

export function selectMode(id: string): void {
  const m = allModes.find(id);
  if (m !== undefined) {
    allModes.setValue(m);
  }
}

export function focusMode(id: string): void {
  const m = allModes.find(id);
  if (m !== undefined) {
    allModes.setValue(m);
    const { enabled = true } = m;
    if (enabled) focus.emit();
  }
}

// --------------------------------------------------------------------------
// --- Search Mode Selector
// --------------------------------------------------------------------------

const switchMode: ModeProps = {
  id: 'ivette.switchmode',
  icon: 'TUNINGS',
  title: 'Search & Action Modes',
  placeholder: 'search mode',
  hints: () => allModes.selfhints(),
  onHint: (h: Hint) => focusMode(h.id),
};

allModes.register(switchMode);

// --------------------------------------------------------------------------
// --- Search Action Component
// --------------------------------------------------------------------------

function lookupHint(h: Hint, lp: string): boolean {
  const hn = h.name ?? h.label;
  return hn ? hn.toLowerCase().includes(lp) : false;
}

function toHint(h: Hint): Toolbar.Hint {
  const label = h.label ?? h.name ?? String(h.id);
  return { ...h, label };
}

function lookupHints(hs: Hint[], pattern: string): Toolbar.Hint[] {
  const p = pattern.toLowerCase();
  return hs.filter((h) => lookupHint(h, p)).map(toHint);
}

export function SearchField(): JSX.Element {
  const [mode] = useGlobalState(allModes);
  const currMode = mode.id;
  const userMode = React.useRef('');
  const [pattern, onPattern] = React.useState('');
  const disabled = mode.id === '';
  const { hints: getHints, enabled = true } = mode;
  const hints = React.useMemo(() => {
    if (!getHints) return [];
    return lookupHints(getHints(), pattern);
  }, [getHints, pattern]);
  React.useEffect(() => {
    if (currMode && currMode !== switchMode.id)
      userMode.current = currMode;
  }, [currMode]);
  const onSearch = React.useCallback(() => {
    focusMode(switchMode.id);
  }, []);
  const onBlur = React.useCallback(() => {
    if (currMode === switchMode.id) {
      const user = findMode(userMode.current) ?? defaultMode;
      allModes.setValue(user);
    }
  }, [currMode]);
  return (
    <Toolbar.SearchField
      icon={mode.icon}
      title={mode.title}
      className={mode.className}
      placeholder={mode.placeholder}
      hints={hints}
      onHint={mode.onHint}
      onEnter={mode.onEnter}
      onPattern={onPattern}
      disabled={disabled}
      enabled={enabled}
      onSearch={onSearch}
      onBlur={onBlur}
      focus={focus}
    />
  );
}

// --------------------------------------------------------------------------
