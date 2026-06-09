/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Lab View Component                                                 --- */
/* -------------------------------------------------------------------------- */

/**
   @packageDocumentation
   @module ivette
 */

import React from 'react';
import { DEVEL } from 'dome';
import { Label } from 'dome/controls/labels';
import { DefineElement } from 'dome/layout/dispatch';
import { Inset } from 'dome/frame/toolbars';
import { ChapterProps, docHistory, HelpButton } from 'dome/help';
import * as State from './state';
import * as Search from './search';
import doc from './doc.md?raw';
import sandoxDoc from '../sandbox/sandbox.md?raw';

// --------------------------------------------------------------------------
// --- help
// --------------------------------------------------------------------------

export const docIvette = { id: "ivette", content: doc, rank: 0 };
docHistory.setValue(['ivette']);

/* -------------------------------------------------------------------------- */
/* --- Items                                                              --- */
/* -------------------------------------------------------------------------- */

export interface ItemProps {
  /** Identifier. */
  id: string;
  /** Displayed name. */
  label: string;
  /** Tooltip description. */
  title?: string;
  /** Ordering index. */
  rank?: number;
}

export interface ContentProps extends ItemProps {
  /** Contents. */
  children?: React.ReactNode;
}

/* -------------------------------------------------------------------------- */
/* --- Groups                                                             --- */
/* -------------------------------------------------------------------------- */

/** @ignore */
export const GROUP = new State.ElementRack<ItemProps>();

/** Defines a group of components.

   The group with identifier `G` contains
   implicitly all components identified by pattern `G.*`. For instance,
   component `fc.kernel.ast` belongs to group `kernel`.

   Group `fc.kernel` is dedicated to components of the Kernel.
   Group `fc.<plugin>` is dedicated to components of plugin `<plugin>`.
   Groups `ivette` and `sandbox` are reserved for Ivette usage.

 */
export function registerGroup(group: ItemProps): void {
  GROUP.register(group);
}

/* -------------------------------------------------------------------------- */
/* --- View Layout                                                        --- */
/* -------------------------------------------------------------------------- */

/** Component identifier. */
export type compId = string;

/** Four elements layout */
export type Layout4 = { A: compId, B: compId, C: compId, D: compId };

/** Three elements layout: one component spreads over two quarters. */
export type Layout3 =
  | { AB: compId, C: compId, D: compId }
  | { AC: compId, B: compId, D: compId }
  | { A: compId, B: compId, CD: compId }
  | { A: compId, BD: compId, C: compId }

/** Two elements layout: each component spreads over two quarters. */
export type Layout2 =
  | { AB: compId, CD: compId }
  | { AC: compId, BD: compId }

/** One elements layout: a single component spreads over all quarters. */
export type Layout1 =
  | { ABCD: compId }

/** A layout displays one to four components. */
export type Layout = Layout1 | Layout2 | Layout3 | Layout4;

/** A view dispatches elements over a predefined layout. */
export interface ViewLayoutProps extends ItemProps {
  /** Use this view by default. */
  defaultView?: boolean;
  /** View layout. */
  layout: Layout;
}

/** @ignore */
export const VIEW = new State.ElementRack<ViewLayoutProps>();

/** Register a new View. */
export function registerView(view: ViewLayoutProps): void {
  VIEW.register(view);
}

/* -------------------------------------------------------------------------- */
/* --- Components                                                         --- */
/* -------------------------------------------------------------------------- */

export type LayoutPosition =
  | 'A' | 'B' | 'C' | 'D'
  | 'AB' | 'AC' | 'BD' | 'CD'
  | 'ABCD';

export interface ComponentProps extends ContentProps {
  /** Icon of the Component */
  icon?: string;
  /** ID of the component documentation  */
  help?: string;
  /** Defaults to 'D' */
  preferredPosition?: LayoutPosition;
}

/** @ignore */
export const COMPONENT = new State.ElementRack<ComponentProps>();

/**
   Register the given Ivette Component.
   Components are sorted by rank and identifier among each group.
 */
export function registerComponent(props: ComponentProps): void {
  COMPONENT.register(props);
}

/* -------------------------------------------------------------------------- */
/* --- TitleBar Component                                                 --- */
/* -------------------------------------------------------------------------- */

/** @ignore */
export interface TitleContext {
  id?: string;
  icon?: string;
  label?: string;
  title?: string;
  help?: string;
}

/** @ignore */
export const TitleContext = React.createContext<TitleContext>({});

export interface TitleBarProps {
  /** Displayed icon. */
  icon?: string;
  /** Displayed name (when mounted). */
  label?: string;
  /** Tooltip description (when mounted). */
  title?: string;
  /** ID of the documentation  */
  help?: string;
  /** TitleBar additional components (stacked to right). */
  children?: React.ReactNode;
}

/**
   LabView Component's title bar.
   Defines an alternative component title bar in current context.
   Default values are taken from the associated component.
 */
export function TitleBar(props: TitleBarProps): JSX.Element | null {
  const context = React.useContext(TitleContext);
  if (!context.id) return null;
  const icon = props.icon || context.icon;
  const label = props.label || context.label;
  const title = props.title || context.title;
  const help = props.help || context.help;
  return (
    <DefineElement id={`labview.title.${context.id}`}>
      <Label
        className="labview-handle"
        icon={icon}
        label={label}
        title={title}
      />
      {props.children}
      {help && <HelpButton id={help} />}
      <Inset />
    </DefineElement>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Side Panels                                                        --- */
/* -------------------------------------------------------------------------- */

export interface SidebarProps extends ContentProps {
  icon?: string;
}

export interface ToolProps {
  id: string;
  rank?: number;
  children?: React.ReactNode;
}

/** @ignore */
export const SIDEBAR = new State.ElementRack<SidebarProps>();

/** @ignore */
export const TOOLBAR = new State.ElementRack<ToolProps>();

/** @ignore */
export const STATUSBAR = new State.ElementRack<ToolProps>();

/** @ignore */
export const DOCCHAPTER = new State.ElementRack<ChapterProps>();

export function registerSidebar(sidebar: SidebarProps): void {
  SIDEBAR.register(sidebar);
}

export function registerToolbar(tools: ToolProps): void {
  TOOLBAR.register(tools);
}

export function registerStatusbar(status: ToolProps): void {
  STATUSBAR.register(status);
}

/** The new chapter is recorded with an error if its Id already exist. */
export function registerDocChapter(chapter: ChapterProps): void {
  DOCCHAPTER.register(chapter);
}

registerDocChapter(docIvette);

/* -------------------------------------------------------------------------- */
/* --- Search Modes                                                       --- */
/* -------------------------------------------------------------------------- */

export interface Hint {
  id: string;
  name?: string; // searched string
  icon?: string; // displayed icon
  label?: string; // displayed hint
  title?: string; // tooltip for hint
  rank?: number; // hint sorting
  onClick?: () => void; // click on hint
}

export interface SearchProps {
  id: string; // Mode identifier
  rank?: number; // Modes ordering
  icon?: string; // Search Field's Icons
  label?: string; // Search Field in mode menu
  title?: string; // Search Field tooltip
  placeholder?: string; // Empty Search Field
  enabled?: boolean; // Search Field input
  className?: string; // Search Field Icon's class
  hints?: () => Hint[]; // Hint sub-menu
  onHint?: (hint: Hint) => void; // Hint selection
  onEnter?: (pattern: string) => void; // Enter key for search field
}

/* eslint-disable max-len */
export function registerSearchMode(m: SearchProps): void { Search.registerMode(m); }
export function updateSearchMode(m: SearchProps): void { Search.updateMode(m); }
export function removeSearchMode(id: string): void { Search.removeMode(id); }
export function selectSearchMode(id: string): void { Search.selectMode(id); }
export function focusSearchMode(id: string): void { Search.focusMode(id); }
/* eslint-enable max-len */

export function useSearchMode(m: SearchProps): void {
  React.useEffect(() => {
    const id = m.id;
    const m0 = Search.findMode(id);
    Search.registerMode({ ...m0, ...m });
    return () => {
      if (m0 !== undefined)
        Search.registerMode(m0);
      else
        Search.removeMode(id);
    };
  }, [m]);
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

if (DEVEL) {
  registerView({
    id: 'sandbox',
    label: 'Sandbox',
    title: 'Sandbox Playground (only in DEVEL mode)',
    layout: { ABCD: 'sandbox.qsplit' },
  });
  registerDocChapter({
    id: 'sandbox',
    content: sandoxDoc,
    rank: 1000
  });
}

export function registerSandbox(props: ComponentProps): void {
  if (DEVEL) {
    if (!props.id.startsWith('sandbox.')) {
      // eslint-disable-next-line no-console
      console.error('SANDBOX wrong identifier', props.id);
    }
    registerComponent(props);
  }
}

/* -------------------------------------------------------------------------- */
