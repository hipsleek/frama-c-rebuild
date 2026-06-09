/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import equal from 'react-fast-compare';
import * as Dome from 'dome';
import * as Json from 'dome/data/json';
import * as States from 'dome/data/states';
import * as Settings from 'dome/data/settings';
import * as Sidebars from 'dome/frame/sidebars';
import * as Toolbar from 'dome/frame/toolbars';
import { Icon } from 'dome/controls/icons';
import { IconButton } from 'dome/controls/buttons';
import { Label } from 'dome/controls/labels';
import { Hbox, Hfill, Vfill, Grid } from 'dome/layout/boxes';
import { QPane, QSplit } from 'dome/layout/qsplit';
import { RenderElement } from 'dome/layout/dispatch';
import { Catch } from 'dome/errors';
import { classes } from 'dome/misc/utils';
import { HelpButton } from 'dome/help';
import * as Ivette from 'ivette';
import { compId, LayoutPosition, VIEW, COMPONENT, GROUP } from 'ivette';
import { NotificationTimer } from 'ivette/prefs';
import * as State from 'ivette/state';

/* -------------------------------------------------------------------------- */
/* --- LabView State                                                      --- */
/* -------------------------------------------------------------------------- */

type tabKey = string;
type viewId = string;

export interface Split { HTOP: number, HBOTTOM: number, V: number }
export interface Layout { A: compId, B: compId, C: compId, D: compId }

export interface TabViewState {
  key: tabKey, /* viewId@custom for custom, or viewId */
  viewId: viewId,
  custom: number, /* 0: normal, n>0: custom */
  split: Split,
  stack: Layout[], /* current at index 0 */
}

export interface LabViewState {
  split: Split;
  stack: Layout[];
  panels: Set<compId>;
  alerts: Set<compId>;
  docked: Map<compId, LayoutPosition>;
  tabs: Map<tabKey, TabViewState>;
  tabKey: tabKey;
  sideView: viewId; // from Sidebar or TAB selection
  sideComp: compId; // from Sidebar selection
}

const defaultSplit: Split = { HTOP: 0.5, HBOTTOM: 0.5, V: 0.5 };
const defaultLayout: Layout = { A: '', B: '', C: '', D: '' };

const LAB = new States.GlobalState<LabViewState>({
  split: defaultSplit,
  stack: [defaultLayout],
  panels: new Set(),
  alerts: new Set(),
  docked: new Map(),
  tabs: new Map(),
  tabKey: '',
  sideView: '',
  sideComp: '',
});

/* -------------------------------------------------------------------------- */
/* --- Settings Management                                                --- */
/* -------------------------------------------------------------------------- */

interface TabSettings {
  view: viewId,
  custom: number,
  split: Split,
  layout: Layout,
}

interface DockSettings {
  comp: compId,
  position: Ivette.LayoutPosition,
}

interface LabSettings {
  tabIndex: number;
  tabs: TabSettings[];
  dock: DockSettings[];
}

const jLayout: Json.Decoder<Layout> =
  Json.jObject({
    A: Json.jString,
    B: Json.jString,
    C: Json.jString,
    D: Json.jString,
  });

const jSplit: Json.Decoder<Split> =
  Json.jObject({
    HTOP: Json.jRange(0, 1, 0.5),
    HBOTTOM: Json.jRange(0, 1, 0.5),
    V: Json.jRange(0, 1, 0.5),
  });

const jPosition: Json.Decoder<Ivette.LayoutPosition> =
  (js: Json.json) => {
    switch (js) {
      case 'A': case 'B': case 'C': case 'D':
      case 'AB': case 'AC': case 'BD': case 'CD':
      case 'ABCD':
        return js;
      default:
        return 'D';
    }
  };

const jTabSettings: Json.Decoder<TabSettings> =
  Json.jObject({
    view: Json.jString,
    custom: Json.jNumber,
    split: jSplit,
    layout: jLayout,
  });

const jDockSettings: Json.Decoder<DockSettings> =
  Json.jObject({
    comp: Json.jString,
    position: jPosition,
  });

const jLabSettings: Json.Decoder<LabSettings> =
  Json.jObject({
    tabIndex: Json.jNumber,
    tabs: Json.jCatch(Json.jArray(jTabSettings), []),
    dock: Json.jCatch(Json.jArray(jDockSettings), []),
  });

const eLabSettings: Json.Encoder<LabSettings> =
  (s: LabSettings): Json.json => ((s as object) as Json.json);

function labSettings(state: LabViewState): LabSettings {
  const tabs: TabSettings[] = [];
  let tabIndex = -1;
  state.tabs.forEach((tab: TabViewState) => {
    const current = tab.key === state.tabKey;
    if (current) tabIndex = tabs.length;
    tabs.push({
      view: tab.viewId,
      custom: tab.custom,
      split: current ? state.split : tab.split,
      layout: current ? state.stack[0] : tab.stack[0],
    });
  });
  const dock: DockSettings[] = [];
  state.docked.forEach((position, comp) => dock.push({ comp, position }));
  return { tabIndex, tabs, dock };
}

const defaultSettings: LabSettings = { tabIndex: 0, tabs: [], dock: [] };

/* -------------------------------------------------------------------------- */
/* --- Layout Utilities                                                   --- */
/* -------------------------------------------------------------------------- */

function compareLayout(u: Layout, v: Layout): boolean {
  return (
    u.A === v.A &&
    u.B === v.B &&
    u.C === v.C &&
    u.D === v.D
  );
}

function isDefined(m: Layout): boolean {
  return !!m.A || !!m.B || !!m.C || !!m.D;
}

function isComplete(m: Layout): boolean {
  return !m.A && !m.B && !m.C && !m.D;
}

const removeLayout = (compId: compId) => (layout: Layout): Layout => {
  const { A, B, C, D } = layout;
  return {
    A: A !== compId ? A : '',
    B: B !== compId ? B : '',
    C: C !== compId ? C : '',
    D: D !== compId ? D : '',
  };
};

function addLayout(
  layout: Layout, compId: compId, at: LayoutPosition
): Layout {
  switch (at) {
    case 'A': return { ...layout, A: compId };
    case 'B': return { ...layout, B: compId };
    case 'C': return { ...layout, C: compId };
    case 'D': return { ...layout, D: compId };
    case 'AB': return { ...layout, A: compId, B: compId };
    case 'AC': return { ...layout, A: compId, C: compId };
    case 'BD': return { ...layout, B: compId, D: compId };
    case 'CD': return { ...layout, C: compId, D: compId };
    case 'ABCD': return { A: compId, B: compId, C: compId, D: compId };
    default: return layout;
  }
}

function makeViewLayout(view: Ivette.Layout): Layout {
  type Unstructured = {
    A?: compId, B?: compId, C?: compId, D?: compId,
    AB?: compId, AC?: compId, BD?: compId, CD?: compId,
    ABCD?: compId
  };
  const u = view as Unstructured;
  const A: compId = u.A ?? u.AB ?? u.AC ?? u.ABCD ?? '';
  const B: compId = u.B ?? u.AB ?? u.BD ?? u.ABCD ?? '';
  const C: compId = u.C ?? u.AC ?? u.CD ?? u.ABCD ?? '';
  const D: compId = u.D ?? u.CD ?? u.BD ?? u.ABCD ?? '';
  return { A, B, C, D };
}

function unstackLayout(
  layout: Layout,
  stack: Layout[],
): Layout[] {
  let k = 1;
  while (!isComplete(layout) && k < stack.length) {
    const layer = stack[k];
    layout = {
      A: layout.A || layer.A,
      B: layout.B || layer.B,
      C: layout.C || layer.C,
      D: layout.D || layer.D,
    };
    k++;
  }
  return [layout, ...stack];
}

function addLayoutComponent(
  stack: Layout[],
  compId: compId,
  at: LayoutPosition
): Layout[] {
  stack = stack.map(removeLayout(compId)).filter(isDefined);
  const top = stack[0] ?? defaultLayout;
  const layout = addLayout(top, compId, at);
  return unstackLayout(layout, stack);
}

function removeLayoutComponent(stack: Layout[], compId: compId): Layout[] {
  stack = stack.map(removeLayout(compId)).filter(isDefined);
  const top = stack[0] ?? defaultLayout;
  return unstackLayout(top, stack);
}

function getLayoutPosition(
  layout: Layout, compId: compId
): LayoutPosition | undefined {
  const { A, B, C, D } = layout;
  const a = A === compId;
  const b = B === compId;
  const c = C === compId;
  const d = D === compId;
  if (a && b && c && d) return 'ABCD';
  if (a && b) return 'AB';
  if (a && c) return 'AC';
  if (b && d) return 'BD';
  if (c && d) return 'CD';
  if (a) return 'A';
  if (b) return 'B';
  if (c) return 'C';
  if (d) return 'D';
  return undefined;
}

/* -------------------------------------------------------------------------- */
/* --- Tabs Utilities                                                     --- */
/* -------------------------------------------------------------------------- */

function previousTab(tabs: Map<tabKey, TabViewState>, key: tabKey):
  TabViewState | undefined {
  let prev: TabViewState | undefined = undefined;
  let last: TabViewState | undefined = undefined;
  tabs.forEach(t => {
    if (t.key === key) prev = last; else last = t;
  });
  return prev || last;
}

function nextTab(tabs: Map<tabKey, TabViewState>, key: tabKey):
  TabViewState | undefined {
  let next: TabViewState | undefined = undefined;
  let first: TabViewState | undefined = undefined;
  let prev = false;
  tabs.forEach(t => {
    if (first === undefined) first = t;
    if (prev) next = t;
    if (t.key === key) prev = true; else prev = false;
  });
  return next || first;
}

function getTabIndex(tabs: Map<tabKey, TabViewState>, key: tabKey): number {
  let index = -1;
  let curIndex = 0;
  tabs.forEach(t => {
    if (t.key === key) index = curIndex;
    curIndex++;
  });
  return index;
}

function changeTabIndex(
  tabs: Map<tabKey, TabViewState>,
  key: tabKey,
  index: number
): Map<tabKey, TabViewState> {
  const newTabs = new Map<tabKey, TabViewState>();
  const tab = tabs.get(key);
  if (!tab) return tabs;
  let curIndex = 0;
  if (index < 0) newTabs.set(key, tab);
  tabs.forEach(t => {
    if (t.key !== key) {
      if (curIndex === index) newTabs.set(key, tab);
      newTabs.set(t.key, t);
      ++curIndex;
    }
  });
  if (curIndex <= index) newTabs.set(key, tab);
  return newTabs;
}

function newCustom(tabs: Map<tabKey, TabViewState>, viewId: viewId): number {
  let custom = 0;
  tabs.forEach(tab => {
    if (tab.viewId === viewId)
      custom = Math.max(custom, tab.custom);
  });
  return custom + 1;
}

const tabKeyOf = (viewId: viewId, custom: number): tabKey =>
  custom > 0 ? `${viewId}@${custom}` : viewId;

function newTab(
  tabs: Map<tabKey, TabViewState>,
  view: Ivette.ViewLayoutProps,
  custom: number,
): TabViewState {
  const { id: viewId } = view;
  const key = tabKeyOf(viewId, custom);
  const tab = {
    key, viewId, custom,
    split: defaultSplit,
    stack: [makeViewLayout(view.layout)],
  };
  tabs.set(key, tab);
  return tab;
}

function saveTab(
  tabs: Map<tabKey, TabViewState>,
  oldState: LabViewState,
): void {
  const oldKey = oldState.tabKey;
  const toSave = tabs.get(oldKey);
  if (toSave !== undefined) {
    const { stack, split } = oldState;
    tabs.set(oldKey, { ...toSave, stack, split });
  }
}

function addPanels(panels: Set<compId>, layout: Layout): Set<compId> {
  const { A, B, C, D } = layout;
  if (panels.has(A) && panels.has(B) && panels.has(C) && panels.has(D))
    return panels;
  else
    return copySet(panels).add(A).add(B).add(C).add(D);
}

function removeAlerts(alerts: Set<compId>, layout: Layout): Set<compId> {
  const { A, B, C, D } = layout;
  if (alerts.has(A) || alerts.has(B) || alerts.has(C) || alerts.has(D)) {
    const newAlerts = copySet(alerts);
    newAlerts.delete(A);
    newAlerts.delete(B);
    newAlerts.delete(C);
    newAlerts.delete(D);
    return newAlerts;
  }
  return alerts;
}

/* -------------------------------------------------------------------------- */
/* --- LabView Actions                                                    --- */
/* -------------------------------------------------------------------------- */

function copySet<A>(s: Set<A>): Set<A> {
  const r = new Set<A>();
  s.forEach((a) => r.add(a));
  return r;
}

function copyMap<A, B>(m: Map<A, B>): Map<A, B> {
  const u = new Map<A, B>();
  m.forEach((v, k) => u.set(k, v));
  return u;
}

function setCurrentView(viewId: viewId = ''): void {
  const state = LAB.getValue();
  LAB.setValue({ ...state, sideView: viewId, sideComp: '' });
}

function setCurrentComp(compId: compId = ''): void {
  const state = LAB.getValue();
  LAB.setValue({ ...state, sideComp: compId, sideView: '' });
}

function setCurrentNone(): void {
  const state = LAB.getValue();
  LAB.setValue({ ...state, sideComp: '', sideView: '' });
}

function applyTab(key: tabKey): void {
  const state = LAB.getValue();
  const old = state.tabKey;
  if (old === key) return;
  const tab = state.tabs.get(key);
  if (!tab) return;
  const { stack, split } = tab;
  const tabs = copyMap(state.tabs);
  const layout = stack[0] ?? defaultLayout;
  saveTab(tabs, state);
  const panels = addPanels(state.panels, layout);
  const alerts = removeAlerts(state.alerts, layout);
  LAB.setValue({
    ...state,
    panels,
    alerts,
    stack,
    split,
    tabs,
    tabKey: key,
  });
}

function closeTab(key: tabKey): void {
  const state = LAB.getValue();
  const tab = previousTab(state.tabs, key);
  const tabs = copyMap(state.tabs);
  tabs.delete(key);
  if (tab === undefined) {
    LAB.setValue({
      ...state,
      stack: [],
      split: defaultSplit,
      tabs, tabKey: ''
    });
  } else {
    const { key, stack, split } = tab;
    const layout = stack[0] ?? defaultLayout;
    const panels = addPanels(state.panels, layout);
    const alerts = removeAlerts(state.alerts, layout);
    LAB.setValue({
      ...state,
      panels, alerts, stack, split, tabs, tabKey: key
    });
  }
}

function restoreDefault(key: tabKey): void {
  const state = LAB.getValue();
  const tab = state.tabs.get(key);
  if (!tab) return;
  const view = VIEW.getElement(tab.viewId);
  if (!view) return;
  const layout = makeViewLayout(view.layout);
  const tabs = copyMap(state.tabs).set(key, { ...tab, stack: [layout] });
  if (key === state.tabKey) {
    const alerts = removeAlerts(state.alerts, layout);
    LAB.setValue({ ...state, tabs, alerts, stack: [layout] });
  } else {
    LAB.setValue({ ...state, tabs });
  }
}

function applyView(view: Ivette.ViewLayoutProps): void {
  const state = LAB.getValue();
  const viewId = view.id;
  if (state.tabs.has(viewId))
    applyTab(viewId);
  else {
    const layout = makeViewLayout(view.layout);
    const panels = addPanels(state.panels, layout);
    const alerts = removeAlerts(state.alerts, layout);
    const tabs = copyMap(state.tabs);
    const tab = newTab(tabs, view, 0);
    saveTab(tabs, state);
    LAB.setValue({
      ...state,
      panels,
      alerts,
      split: defaultSplit,
      stack: [layout],
      tabs, tabKey: tab.key
    });
  }
}

function duplicateView(view: Ivette.ViewLayoutProps): void {
  const state = LAB.getValue();
  const custom = newCustom(state.tabs, view.id);
  const tabs = copyMap(state.tabs);
  newTab(tabs, view, custom);
  LAB.setValue({ ...state, tabs });
}

function applyComponent(
  comp: Ivette.ComponentProps,
  at?: LayoutPosition
): void {
  const state = LAB.getValue();
  const { id, preferredPosition } = comp;
  const pos = at ?? preferredPosition ?? 'D';
  const stack = addLayoutComponent(state.stack, id, pos);
  const panels = copySet(state.panels).add(id);
  const alerts = copySet(state.alerts);
  alerts.delete(id);
  LAB.setValue({ ...state, panels, alerts, stack });
}

function applyDock(
  comp: Ivette.ComponentProps,
  at?: Ivette.LayoutPosition
): void {
  const { id, preferredPosition } = comp;
  const state = LAB.getValue();
  const top = state.stack[0] ?? defaultLayout;
  const pos =
    at ?? getLayoutPosition(top, id) ?? preferredPosition ?? 'D';
  const stack = removeLayoutComponent(state.stack, id);
  const docked = copyMap(state.docked).set(id, pos);
  LAB.setValue({ ...state, docked, stack });
}

function undockComponent(compId: compId): void {
  const state = LAB.getValue();
  if (state.docked.has(compId)) {
    const docked = copyMap(state.docked);
    const alerts = copySet(state.alerts);
    docked.delete(compId);
    alerts.delete(compId);
    LAB.setValue({ ...state, docked, alerts });
  }
}

function closeComponent(compId: compId): void {
  const state = LAB.getValue();
  const stack = removeLayoutComponent(state.stack, compId);
  const panels = copySet(state.panels);
  const docked = copyMap(state.docked);
  const alerts = copySet(state.alerts);
  panels.delete(compId);
  docked.delete(compId);
  alerts.delete(compId);
  LAB.setValue({ ...state, panels, docked, stack });
}

/* -------------------------------------------------------------------------- */
/* --- Update from Settings                                               --- */
/* -------------------------------------------------------------------------- */

const filterComponent = (id: compId): compId =>
  COMPONENT.getElement(id) !== undefined ? id : '';

const filterLayout = (w: Layout): Layout => ({
  A: filterComponent(w.A),
  B: filterComponent(w.B),
  C: filterComponent(w.C),
  D: filterComponent(w.D),
});

function updateTab(
  newTabs: Map<tabKey, TabViewState>,
  tab: TabSettings
): boolean {
  const { custom, view: viewId } = tab;
  const view = VIEW.getElement(viewId);
  if (!view) return false;
  const tabKey = tabKeyOf(viewId, custom);
  const tabState = newTabs.get(tabKey);
  const oldStack =
    tabState !== undefined ? tabState.stack : [
      defaultLayout, makeViewLayout(view.layout)
    ]; // unstack starts at depth 1
  const stack = unstackLayout(filterLayout(tab.layout), oldStack);
  if (
    tabState === undefined ||
    tabState.custom !== tab.custom ||
    !equal(tabState.stack[0], stack[0]) ||
    !equal(tabState.split, tab.split)
  ) {
    newTabs.set(tabKey, {
      key: tabKey, viewId, custom: tab.custom, stack, split: tab.split,
    });
    return true;
  } else
    return false;
}

function updateDock(
  newDock: Map<compId, LayoutPosition>,
  dock: DockSettings,
): boolean {
  const { comp, position } = dock;
  if (COMPONENT.getElement(comp) === undefined) return false;
  const current = newDock.get(comp);
  if (current !== position) {
    newDock.set(comp, position);
    return true;
  } else
    return false;
}

function updateIndex(settings: LabSettings): void {
  const theTab = settings.tabs[settings.tabIndex];
  if (theTab) {
    applyTab(tabKeyOf(theTab.view, theTab.custom));
  } else {
    const views = VIEW.getElements();
    views.forEach(view => { if (view.defaultView) applyView(view); });
  }
  setCurrentNone();
}

/* -------------------------------------------------------------------------- */
/* --- Settings Update                                                    --- */
/* -------------------------------------------------------------------------- */

let synchronize = true;

LAB.on((state: LabViewState) => {
  if (synchronize) {
    try {
      synchronize = false;
      const data = labSettings(state);
      Settings.setWindowSettings('ivette.laboratory', eLabSettings(data));
    } finally {
      synchronize = true;
    }
  }
});

Settings.onWindowSettings(() => {
  if (synchronize) {
    try {
      synchronize = false;
      const settings = Settings.getWindowSettings(
        'ivette.laboratory', jLabSettings, defaultSettings
      );
      let modified = false;
      const state = LAB.getValue();
      const newTabs = copyMap(state.tabs);
      saveTab(newTabs, state);
      settings.tabs.forEach(tab => {
        if (updateTab(newTabs, tab)) modified = true;
      });
      const newDock = copyMap(state.docked);
      settings.dock.forEach(dock => {
        if (updateDock(newDock, dock)) modified = true;
      });
      if (modified) LAB.setValue({ ...state, tabs: newTabs, docked: newDock });
      if (!state.tabKey) updateIndex( settings );
    } finally {
      synchronize = true;
    }
  }
});

/* -------------------------------------------------------------------------- */
/* --- Exported API                                                       --- */
/* -------------------------------------------------------------------------- */

export function useState(): LabViewState {
  const [state] = States.useGlobalState(LAB);
  return state;
}

export interface ViewStatus {
  displayed: boolean;
  layout: Layout;
}

export function getViewStatus(
  state: LabViewState,
  viewId: viewId
): ViewStatus {
  const tab = state.tabs.get(viewId);
  const displayed = tab ? tab.key === state.tabKey : false;
  const layout = displayed ? state.stack[0] : tab?.stack[0];
  return { displayed, layout: layout ?? defaultLayout };
}

export interface ComponentStatus {
  active: boolean;
  docked: boolean;
  position: Ivette.LayoutPosition | undefined;
}

export function getComponentStatus(
  state: LabViewState,
  compId: compId
): ComponentStatus {
  const layout = state.stack[0] ?? defaultLayout;
  const position = getLayoutPosition(layout, compId);
  const active = state.panels.has(compId);
  const docked = state.docked.has(compId);
  return { position, active, docked };
}

/* -------------------------------------------------------------------------- */
/* --- Layout Menu State                                                  --- */
/* -------------------------------------------------------------------------- */

interface Actions {
  dock: boolean;
  undock: boolean;
  close: boolean;
}

interface LayoutMenuState extends Actions {
  compId: compId;
  x: number;
  y: number;
  fromDock: boolean;
}

const closedMenu: LayoutMenuState = {
  compId: '',
  dock: false,
  undock: false,
  close: false,
  fromDock: false,
  x: 0,
  y: 0,
};

const LAYOUTMENU = new States.GlobalState<LayoutMenuState>(closedMenu);

function openLayoutMenu(
  compId: compId,
  actions: Actions,
  evt: React.MouseEvent,
  fromDock = false
): void {
  LAYOUTMENU.setValue({
    ...actions, compId,
    x: evt.clientX, y: evt.clientY, fromDock
  });
}

function closeMenu(): void {
  LAYOUTMENU.setValue(closedMenu);
}

/* -------------------------------------------------------------------------- */
/* --- Layout Menu Component                                              --- */
/* -------------------------------------------------------------------------- */

interface QuarterProps {
  compId: compId;
  layout: Layout;
  pos: LayoutPosition;
}

function Quarter(props: QuarterProps): JSX.Element {
  const { layout, compId, pos } = props;
  const icon = 'QSPLIT.' + pos;
  const onClick = (): void => {
    closeMenu();
    const comp = COMPONENT.getElement(compId);
    if (comp) applyComponent(comp, pos);
  };
  const curp = getLayoutPosition(layout, compId);
  return (
    <IconButton
      className='labview-layout-quarter'
      icon={icon}
      disabled={curp === pos}
      onClick={onClick} />
  );
}

interface ActionProps {
  icon: string;
  label: string;
  display: boolean;
  onClick: () => void;
}

function Action(props: ActionProps): JSX.Element {
  const { icon, label, display, onClick } = props;
  return (
    <Label
      className='labview-layout-action'
      display={display}
      label={label}
      onClick={onClick}
    >
      <Icon className='labview-layout-action-icon' id={icon} />
    </Label>
  );
}

function LayoutMenu(): JSX.Element | null {
  const href = React.useRef<HTMLDivElement>(null);
  const divElt = href.current;
  const [menu] = States.useGlobalState(LAYOUTMENU);
  const [state] = States.useGlobalState(LAB);
  const [panelWidth, setWidth] = React.useState(80);
  const [panelHeight, setHeight] = React.useState(80);
  const layout = state.stack[0] ?? defaultLayout;
  const { compId, dock, undock, close } = menu;
  const display = compId !== '';

  React.useEffect(() => {
    if (display && divElt) {
      divElt.focus({ preventScroll: true });
    }
  }, [display, divElt]);

  const width = Math.max(divElt?.offsetWidth ?? 0, panelWidth);
  const height = Math.max(divElt?.offsetHeight ?? 0, panelHeight);
  React.useEffect(() => setWidth(width), [width]);
  React.useEffect(() => setHeight(height), [height]);

  const className = classes(
    'dome-color-frame',
    'labview-layout-menu',
    !display && 'dome-erased'
  );

  const maxWidth = window.innerWidth;
  const maxHeight = window.innerHeight;

  const left = Math.max(0, Math.min(menu.x, maxWidth - width));
  const top = Math.max(0, Math.min(menu.y, maxHeight - height));

  const onDock = (): void => {
    closeMenu();
    const comp = COMPONENT.getElement(compId);
    if (comp) applyDock(comp);
  };

  const onUndock = (): void => {
    closeMenu();
    undockComponent(compId);
  };

  const onClose = (): void => {
    closeMenu();
    closeComponent(compId);
  };

  return (
    <div
      ref={href}
      tabIndex={0}
      className={className}
      style={menu.fromDock ? { left, bottom: 0 } : { left, top }}
      onBlur={closeMenu}
      onKeyDown={closeMenu}
    >
      <Grid columns='24px 24px 24px'>
        <Quarter compId={compId} layout={layout} pos='A' />
        <Quarter compId={compId} layout={layout} pos='AB' />
        <Quarter compId={compId} layout={layout} pos='B' />
        <Quarter compId={compId} layout={layout} pos='AC' />
        <Quarter compId={compId} layout={layout} pos='ABCD' />
        <Quarter compId={compId} layout={layout} pos='BD' />
        <Quarter compId={compId} layout={layout} pos='C' />
        <Quarter compId={compId} layout={layout} pos='CD' />
        <Quarter compId={compId} layout={layout} pos='D' />
      </Grid>
      <Action
        display={dock} label='Dock' icon='QSPLIT.DOCK' onClick={onDock} />
      <Action
        display={undock} label='Undock' icon='QSPLIT.DOCK' onClick={onUndock} />
      <Action
        display={close} label='Close' icon='TRASH' onClick={onClose} />
    </div>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Notification Stack                                                 --- */
/* -------------------------------------------------------------------------- */

export type NotificationKind = 'message' | 'warning' | 'error';

export interface Notification {
  kind: NotificationKind;
  label: string;
  title?: string;
}

interface NotificationItemProps extends Notification { id: string }

type NotificationState = {
  kid: number,
  index: Map<string, NotificationItemProps>,
};

const NOTIFICATIONS = new States.GlobalState<NotificationState>({
  kid: 0, index: new Map()
});

function clearMessage(id: string): void {
  let { kid, index } = NOTIFICATIONS.getValue();
  if (!index.has(id)) return;
  index = copyMap(index);
  index.delete(id);
  NOTIFICATIONS.setValue({ kid, index });
}

function NotificationItem(props: NotificationItemProps): JSX.Element {
  const { id, kind = 'message', label, title } = props;
  const className = classes(
    'labview-notification-item',
    'labview-notification-' + kind,
  );
  const icon = props.kind === 'message' ? 'CIRC.INFO' : 'WARNING';
  const onClick = (): void => clearMessage(id);
  return (
    <Label
      className={className}
      icon={icon} title={title} label={label}
      onClick={onClick} />
  );
}

function Notifications(): JSX.Element {
  const [{ index }] = States.useGlobalState(NOTIFICATIONS);
  const className = classes(
    'labview-notification-stack',
    index.size === 0 && 'dome-erased'
  );
  const items: JSX.Element[] = [];
  index.forEach(p => items.push(<NotificationItem key={p.id} {...p} />));
  return <div className={className}>{items}</div>;
}

export function clearMessages(): void {
  const { kid: kid } = NOTIFICATIONS.getValue();
  NOTIFICATIONS.setValue({ kid: kid, index: new Map() });
}

export function showMessage(msg: Notification): void {
  let { kid, index } = NOTIFICATIONS.getValue();
  const id = `W${++kid}`;
  index = copyMap(index).set(id, { ...msg, id });
  NOTIFICATIONS.setValue({ kid, index });
  const timer = Settings.getGlobalSettings(NotificationTimer);
  if (timer > 0 && timer < 60)
    setTimeout(() => clearMessage(id), timer * 1000);
}

/* -------------------------------------------------------------------------- */
/* --- Pane Component                                                     --- */
/* -------------------------------------------------------------------------- */

interface PaneProps { compId: compId }

const paneActions: Actions = { dock: true, undock: false, close: true };

function Pane(props: PaneProps): JSX.Element | null {
  const { compId } = props;
  const component = State.useElement(COMPONENT, compId);
  const onLayout = React.useCallback(
    (evt: React.MouseEvent) => openLayoutMenu(compId, paneActions, evt),
    [compId]
  );
  if (!component) return null;
  const { icon, label, title, help, children } = component;
  const context = { id: compId, icon, label, title, help };
  return (
    <QPane id={compId}>
      <Vfill className="labview-content">
        <Hbox className="labview-titlebar" onContextMenu={onLayout}>
          <Hfill>
            <Catch label={compId}>
              <RenderElement id={`labview.title.${compId}`}>
                <Label
                  className="labview-handle"
                  icon={icon}
                  label={label}
                  title={title} />
                {help && <HelpButton id={help} />}
              </RenderElement>
            </Catch>
          </Hfill>
        </Hbox>
        <Ivette.TitleContext.Provider value={context}>
          <Catch label={compId}>{children}</Catch>
        </Ivette.TitleContext.Provider>
      </Vfill>
    </QPane>
  );
}

/* -------------------------------------------------------------------------- */
/* --- LabView                                                            --- */
/* -------------------------------------------------------------------------- */

export function LabView(): JSX.Element {
  const [state] = States.useGlobalState(LAB);
  const setPosition = React.useCallback(
    (HTOP: number, HBOTTOM: number, V: number) =>
      LAB.setValue({ ...state, split: { HTOP, HBOTTOM, V } }),
    [state]
  );
  const layout = state.stack[0] ?? defaultLayout;
  const { A, B, C, D } = layout;
  const { HTOP, HBOTTOM, V } = state.split;
  const panels: JSX.Element[] = [];
  state.panels.forEach((id) => panels.push(<Pane key={id} compId={id} />));
  return (
    <>
      <LayoutMenu />
      <Notifications />
      <QSplit
        className='labview-container'
        A={A} B={B} C={C} D={D} HTOP={HTOP} HBOTTOM={HBOTTOM} V={V}
        setPosition={setPosition}
      >{panels}</QSplit>
    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- View Sidebar Section                                               --- */
/* -------------------------------------------------------------------------- */

export interface ViewItemProps {
  view: Ivette.ViewLayoutProps;
  selected: boolean;
  displayed: boolean;
  layout: Layout | undefined;
}

export function ViewItem(props: ViewItemProps): JSX.Element {
  const { view, displayed, selected, layout } = props;
  const { id, label: vname, title: vtitle } = view;

  const onSelection = (_evt: React.MouseEvent): void => {
    setCurrentView(id);
    applyView(view);
  };

  const icon = 'DISPLAY';
  const modified =
    (layout !== undefined &&
      !compareLayout(layout, makeViewLayout(view.layout)));

  const label = modified ? vname + '*' : vname;
  const tname = vtitle || vname;
  const title = modified ? tname + ' (modified)' : tname;

  const onContextMenu = (): void => {
    setCurrentView(id);
    const onDisplay = (): void => applyView(view);
    const onRestore = (): void => restoreDefault(view.id);
    const onDuplicate = (): void => duplicateView(view);
    Dome.popupMenu([
      { label: 'Display View', enabled: !displayed, onClick: onDisplay },
      { label: 'Duplicate View', onClick: onDuplicate },
      { label: 'Restore Default', enabled: modified, onClick: onRestore },
    ]);
  };

  return (
    <Sidebars.Item
      key={id}
      icon={icon}
      label={label}
      title={title}
      selected={selected}
      onSelection={onSelection}
      onContextMenu={onContextMenu}
    />
  );
}

function ViewSection(): JSX.Element {
  const views = State.useElements(VIEW);
  const [{ tabs, tabKey, sideView, stack }] = States.useGlobalState(LAB);
  const items = views.map((view) => {
    const { id } = view;
    const tab = tabs.get(id);
    const displayed = tab ? tab.key === tabKey : false;
    const layout = displayed ? stack[0] : tab?.stack[0];
    return (
      <ViewItem
        key={id}
        view={view}
        layout={layout}
        displayed={displayed}
        selected={id === sideView} />
    );
  });

  return (
    <Sidebars.Section
      settings="ivette.sidebar.views" label="Views" defaultUnfold
    >
      {items}
    </Sidebars.Section>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Component Sidebar Item                                             --- */
/* -------------------------------------------------------------------------- */

export interface ComponentItemProps {
  comp: Ivette.ComponentProps;
  position: LayoutPosition | undefined;
  selected: boolean;
  active: boolean;
  docked: boolean;
}

export function ComponentItem(props: ComponentItemProps): JSX.Element {
  const { comp, position, selected, active, docked } = props;
  const { id, label, title = label } = comp;
  const icon =
    position ? 'QSPLIT.' + position :
      docked ? 'QSPLIT.DOCK' :
        'COMPONENT';

  const mlabel = !position && active ? label + '*' : label;

  const status =
    position ? 'Visible' :
      docked ? 'Docked' :
        active ? 'Running' : 'Closed';

  const onSelection = (): void => {
    setCurrentComp(id);
  };

  const onDoubleClick = (): void => {
    setCurrentComp(id);
    applyComponent(comp);
  };

  const onContextMenu = (evt: React.MouseEvent): void => {
    setCurrentComp(id);
    openLayoutMenu(id, { dock: !docked, undock: docked, close: active }, evt);
  };

  return (
    <Sidebars.Item
      icon={icon}
      label={mlabel}
      title={`${title} (${status})`}
      onSelection={onSelection}
      onDoubleClick={onDoubleClick}
      onContextMenu={onContextMenu}
      selected={selected}
    />
  );
}

/* -------------------------------------------------------------------------- */
/* --- Group Sidebar Section                                              --- */
/* -------------------------------------------------------------------------- */

export interface ID { id: string }

export const inGroup = (g: ID) => (e: ID) => e.id.startsWith(g.id + '.');
export const groupOf = (e: ID) => (g: ID) => e.id.startsWith(g.id + '.');
export const inNoGroup = (gs: ID[]) => (e: ID) => !gs.some(groupOf(e));

interface GroupSectionProps extends Ivette.ItemProps {
  filter: (comp: ID) => boolean;
}

function GroupSection(props: GroupSectionProps): JSX.Element | null {
  const { id, label, title, filter } = props;
  const settings = 'ivette.sidebar.group.' + id;
  const components = State.useElements(COMPONENT).filter(filter) ?? [];
  const [{ panels, docked, sideComp, stack }] = States.useGlobalState(LAB);
  const layout = stack[0] ?? defaultLayout;
  const items = components.map((comp) => {
    const { id } = comp;
    return (
      <ComponentItem
        key={id}
        comp={comp}
        position={getLayoutPosition(layout, id)}
        selected={id === sideComp}
        active={panels.has(id)}
        docked={docked.has(id)}
      />
    );
  });
  return (
    <Sidebars.Section settings={settings} label={label} title={title}>
      {items}
    </Sidebars.Section>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Views & Components Sidebar                                         --- */
/* -------------------------------------------------------------------------- */

const Components: Ivette.ItemProps = {
  id: 'components',
  label: 'Other Plugins',
  title: 'Components from other Frama-C Plugins'
};

const Sandbox: Ivette.ItemProps = {
  id: 'sandbox',
  label: 'Sandbox',
  title: 'Sandbox Components (dev mode only)'
};

function ViewBar(): JSX.Element {
  const groups = State.useElements(GROUP);
  const allGroups = groups.concat(Sandbox);

  return (<>
      <Sidebars.SidebarTitle label='Views & Components' />
      <div className="globals-scrollable-area">
        <ViewSection key='views' />
        {groups.map((group) =>
          <GroupSection
            key={group.id}
            filter={inGroup(group)} {...group} />)}
        <GroupSection
          key='components'
          filter={inNoGroup(allGroups)} {...Components} />
        <GroupSection
          key='sandbox'
          filter={inGroup(Sandbox)} {...Sandbox} />
      </div>
    </>
  );
}

Ivette.registerSidebar({
  id: 'ivette.views',
  label: 'Views',
  icon: 'DISPLAY',
  title: 'Views & Components Selector',
  children: <ViewBar />,
});

// --------------------------------------------------------------------------
// --- Docked Components
// --------------------------------------------------------------------------

interface DockItemProps {
  compId: compId;
  visible: boolean;
  alert: boolean;
  position: LayoutPosition;
}

function DockItem(props: DockItemProps): JSX.Element | null {
  const { compId, visible, position, alert } = props;
  const comp = State.useElement(COMPONENT, compId);
  if (comp === undefined) return null;
  const label = comp.label ?? compId;
  const icon = 'QSPLIT.' + position;
  const title = `Display ${label} (right-click for more actions)`;

  const className = classes(
    'labview-docked', visible && 'disabled', alert && 'alert'
  );

  const onClick = (): void => {
    if (visible) {
      applyDock(comp);
    } else {
      applyComponent(comp, position);
    }
    setCurrentNone();
  };

  const onContextMenu = (evt: React.MouseEvent): void => {
    openLayoutMenu(compId, {
      dock: visible,
      undock: true,
      close: true
    }, evt, true);
  };

  return (
    <Label
      className={className}
      icon={icon}
      label={label}
      title={title}
      onClick={onClick}
      onContextMenu={onContextMenu}
    />
  );
}

export function Dock(): JSX.Element {
  const [{ docked, stack, alerts }] = States.useGlobalState(LAB);
  const items: JSX.Element[] = [];
  docked.forEach((pos, compId) => {
    const layout = stack[0] ?? defaultLayout;
    const curr = getLayoutPosition(layout, compId);
    items.push(
      <DockItem
        key={compId}
        compId={compId}
        visible={curr !== undefined}
        alert={alerts.has(compId)}
        position={curr ?? pos}
      />);
  });
  return <>{items}</>;
}

/* -------------------------------------------------------------------------- */
/* --- Tabs                                                               --- */
/* -------------------------------------------------------------------------- */

interface TabViewProps {
  tab: TabViewState;
  tabKey: tabKey;
  layout: Layout;
}

function TabView(props: TabViewProps): JSX.Element | null {
  const { tab, tabKey } = props;
  const { viewId, custom, key } = tab;
  const view = State.useElement(VIEW, viewId);
  if (!view) return null;
  const selected = key === tabKey;
  const top = tab.stack[0] ?? defaultLayout;
  const layout = selected ? props.layout : top;
  const modified = !compareLayout(layout, makeViewLayout(view.layout));
  const vname = view.label;
  const tname = custom > 0 ? `${vname} ~ ${custom}` : vname;
  const label = modified ? `${tname}*` : tname;
  const tdup = custom > 0 ? 'Custom ' : '';
  const tmod = modified ? ' (modified)' : '';
  const title = tdup + vname + tmod;

  const onClick = (): void => { applyTab(key); setCurrentNone(); };
  const onClose = (): void => closeTab(key);
  const onContextMenu = (): void => {
    const onDisplay = (): void => applyTab(key);
    const onRestore = (): void => restoreDefault(key);
    Dome.popupMenu([
      { label: 'Display View', enabled: !selected, onClick: onDisplay },
      { label: 'Restore Default', enabled: modified, onClick: onRestore },
      { label: 'Close Tab', display: custom <= 0, onClick: onClose },
    ]);
  };
  const onAuxClick = (_value: undefined, event: React.MouseEvent): void => {
    if (event.button === 1) onClose();
  };

  const icon = 'DISPLAY';

  return (
    <Toolbar.Button
      className='labview-tab'
      icon={icon}
      label={label}
      title={title}
      selected={selected}
      onClick={onClick}
      onContextMenu={onContextMenu}
      onAuxClick={onAuxClick}
    >
      <IconButton
        className='labview-tab-closing'
        icon='CIRC.CLOSE'
        onClick={onClose}
      />
    </Toolbar.Button>
  );
}

export function Tabs(): JSX.Element {
  const [{ tabKey, stack, tabs }] = States.useGlobalState(LAB);
  const layout = stack[0] ?? defaultLayout;
  const items: JSX.Element[] = [];
  tabs.forEach((tab: TabViewState) =>
    items.push(
      <TabView
        key={tab.key}
        tab={tab}
        tabKey={tabKey}
        layout={layout}
      />
    ));
  return <>{items}</>;
}

export function switchToView(id: string): void {
  const view = VIEW.getElement(id);
  if (view) applyView(view);
}

export function showComponent(id: string, at?: LayoutPosition): void {
  const comp = COMPONENT.getElement(id);
  if (comp) applyComponent(comp, at);
}

export function dockComponent(id: string, at?: LayoutPosition): void {
  const comp = COMPONENT.getElement(id);
  if (comp) applyDock(comp, at);
}

export function alertComponent(id: string): void {
  const comp = COMPONENT.getElement(id);
  if (!comp) return;
  const state = LAB.getValue();
  /* Do nothing if the component if already visible. */
  const layout = state.stack[0] ?? defaultLayout;
  const curr = getLayoutPosition(layout, id);
  if (curr !== undefined) return;
  /* Add the component to the set of alerted components. */
  const alerts = copySet(state.alerts).add(id);
  LAB.setValue({ ...state, alerts });
  /* Dock the component if it isn't already. */
  if (state.docked.has(id)) return;
  applyDock(comp);
}

/* -------------------------------------------------------------------------- */
/* --- Search Mode                                                        --- */
/* -------------------------------------------------------------------------- */

function displayHints(): Ivette.Hint[] {
  const hints: Ivette.Hint[] = [];
  VIEW.getElements().forEach((view) => hints.push({
    id: 'view#' + view.id,
    name: view.label,
    icon: 'DISPLAY',
    label: view.label,
    title: view.title,
    onClick: () => applyView(view),
  }));
  COMPONENT.getElements().forEach((comp) => hints.push({
    id: 'comp#' + comp.id,
    name: comp.label,
    icon: 'COMPONENT',
    label: comp.label,
    title: comp.title,
    onClick: () => applyComponent(comp),
  }));
  return hints;
}

Ivette.registerSearchMode({
  id: 'ivette.show',
  label: 'Show',
  title: 'Select Views & Components',
  icon: 'DISPLAY',
  hints: displayHints,
});

Dome.addMenuItem({
  menu: 'View',
  id: 'ivette.show',
  label: 'Show View or Component …',
  key: 'Cmd+K',
  onClick: () => Ivette.focusSearchMode('ivette.show'),
});

/* -------------------------------------------------------------------------- */
/* --- Menu and shortcuts for tab selection and movements                 --- */
/* -------------------------------------------------------------------------- */

function applyPrevTab(): void {
  const state = LAB.getValue();
  const tab = previousTab(state.tabs, state.tabKey);
  if (tab) applyTab(tab.key);
}

function applyNextTab(): void {
  const state = LAB.getValue();
  const tab = nextTab(state.tabs, state.tabKey);
  if (tab) applyTab(tab.key);
}

Dome.addMenuItem({
  menu: 'View',
  id: 'ivette.tab.next',
  label: 'Select next tab',
  key: 'Cmd+Tab',
  onClick: applyNextTab,
});

Dome.addMenuItem({
  menu: 'View',
  id: 'ivette.tab.prev',
  label: 'Select previous tab',
  key: 'Cmd+Shift+Tab',
  onClick: applyPrevTab,
});

/* The invisible menu items below are added to create more shortcuts for tab
   selection. These menu items should be removed when shortcuts can be added
   without creating dedicated menu items. */

Dome.addMenuItem({
  menu: 'View',
  id: 'ivette.tab.next2',
  label: 'Select next tab',
  key: 'Cmd+PageDown',
  visible: false,
  onClick: applyNextTab,
});

Dome.addMenuItem({
  menu: 'View',
  id: 'ivette.tab.prev2',
  label: 'Select previous tab',
  key: 'Cmd+PageUp',
  visible: false,
  onClick: applyPrevTab,
});

function nthTab(tabs: Map<tabKey, TabViewState>, i: number):
  TabViewState | undefined {
  let tab = undefined;
  let count = 1;
  tabs.forEach(t => {
    if (count === i) tab = t;
    count++;
  });
  return tab;
}

function applyNthTab(i: number): void {
  const state = LAB.getValue();
  const tab = nthTab(state.tabs, i);
  if (tab) applyTab(tab.key);
}

/* These shortcuts work on qwerty, azerty and dvorak keyboard layouts,
   but not always on more exotic ones, such as bepo. */
function addShortcutToNthTab(i: number): void {
  Dome.addMenuItem({
    menu: 'View',
    id: `ivette.tab.${i}`,
    label: `Select ${i}-nth tab`,
    visible: false,
    key: `Cmd+${i}`,
    onClick: () => applyNthTab(i),
  });
}

for (let i = 0; i < 10; i++) {
  addShortcutToNthTab(i);
}

function moveTab(shift: number): void {
  const state = LAB.getValue();
  const tabIndex = getTabIndex(state.tabs, state.tabKey);
  const tabs = changeTabIndex(state.tabs, state.tabKey, tabIndex + shift);
  // Force the change: tabs are the same, but their order has changed.
  LAB.setValue({ ...state, tabs }, true);
}

function moveTabBackward(): void {
  moveTab(-1);
}
function moveTabForward(): void {
  moveTab(1);
}

Dome.addMenuItem({
  menu: 'View',
  id: 'ivette.tab.moveForward',
  label: 'Move tab forward',
  key: 'Cmd+Shift+PageDown',
  onClick: moveTabForward,
});

Dome.addMenuItem({
  menu: 'View',
  id: 'ivette.tab.moveBackward',
  label: 'Move tab backward',
  key: 'Cmd+Shift+PageUp',
  onClick: moveTabBackward,
});

/* -------------------------------------------------------------------------- */
