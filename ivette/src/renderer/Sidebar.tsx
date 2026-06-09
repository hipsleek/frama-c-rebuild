/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Sidebar
// --------------------------------------------------------------------------

import React from 'react';
import * as Dome from 'dome';
import { Icon } from 'dome/controls/icons';
import { SideBar } from 'dome/frame/sidebars';
import { Catch } from 'dome/errors';
import { classes } from 'dome/misc/utils';
import { SidebarProps, SIDEBAR } from 'ivette';
import * as State from 'ivette/state';

export const DEFAULT_SIDEBAR_PANEL_WIDTH = 320;
const SIDEBAR_TOGGLE_MENU_ID = 'ivette.sidebar.toggle';

/* -------------------------------------------------------------------------- */
/* --- Sidebar State                                                      --- */
/* -------------------------------------------------------------------------- */

/** Selection-related sidebar state shared by selectors and panels. */
export interface SidebarSelectionState {
  selectorSelected: string;
  setSelectorSelected: (selector: string) => void;
  registeredSidebars: SidebarProps[];
}

/** Control for the collapsible sidebar panel. */
export interface SidebarPanel {
  visible: boolean;
  setVisible: (visible: boolean) => void;
}

/** Aggregate sidebar state exposed to the main application layout. */
export interface SidebarState {
  selection: SidebarSelectionState;
  panel: SidebarPanel;
}

/**
   Hook responsible for the currently selected sidebar and the list
   of registered sidebars, sorted by rank.
 */
function useSidebarSelectionState(): SidebarSelectionState {
  const [selectorSelected, setSelectorSelected] =
    Dome.useStringSettings('ivette.sidebar.selected');
  const registeredSidebars = State.useElements(SIDEBAR);
  const sortedSidebars = React.useMemo(
    () => [...registeredSidebars].sort((a, b) => (a.rank ?? 0) - (b.rank ?? 0)),
    [registeredSidebars],
  );

  // Ensure the selected sidebar always refers to a currently registered one.
  React.useEffect(() => {
    if (sortedSidebars.every((sb) => sb.id !== selectorSelected)) {
      const first = sortedSidebars[0];
      if (first) setSelectorSelected(first.id);
    }
  }, [sortedSidebars, selectorSelected, setSelectorSelected]);

  return React.useMemo(
    () => ({
      selectorSelected,
      setSelectorSelected,
      registeredSidebars: sortedSidebars,
    }),
    [selectorSelected, setSelectorSelected, sortedSidebars],
  );
}

/** Hook to control whether the sidebar panel is visible. */
function useSidebarPanel(): SidebarPanel {
  const [panelVisible, setPanelVisible] =
    Dome.useBoolSettings('frama-c.sidebar.unfold', true);

  return React.useMemo(
    () => ({
      visible: panelVisible,
      setVisible: setPanelVisible,
    }),
    [panelVisible, setPanelVisible],
  );
}

/** Hook to gather the sidebar selection and panel control state. */
export function useSidebarState(): SidebarState {
  const selection = useSidebarSelectionState();
  const panel = useSidebarPanel();

  return React.useMemo(
    () => ({ selection, panel }),
    [selection, panel],
  );
}

/** Hook to register and maintain sidebar-related menu shortcuts. */
export function useSidebarShortcuts(sidebarPanel: SidebarPanel): void {
  React.useEffect(() => {
    Dome.addMenuItem({
      menu: 'View',
      id: SIDEBAR_TOGGLE_MENU_ID,
      label: 'Toggle Sidebar',
      key: 'Cmd+B',
    });
    return () => {
      Dome.setMenuItem({ id: SIDEBAR_TOGGLE_MENU_ID, onClick: null });
    };
  }, []);

  React.useEffect(() => {
    Dome.setMenuItem({
      id: SIDEBAR_TOGGLE_MENU_ID,
      onClick: () => sidebarPanel.setVisible(!sidebarPanel.visible),
    });
  }, [sidebarPanel]);
}

/* -------------------------------------------------------------------------- */
/* --- Sidebar Classic Selector                                           --- */
/* -------------------------------------------------------------------------- */

interface SelectorProps extends SidebarProps, SidebarState { }

function Selector(props: SelectorProps): JSX.Element {
  const { id, icon, selection, panel, label } = props;
  const { selectorSelected, setSelectorSelected } = selection;
  const { visible, setVisible } = panel;
  const className = classes(
    'sidebar-selector',
    'dome-color-frame',
    selectorSelected === id && 'sidebar-selector-selected',
  );
  const onClick = React.useCallback(() => {
    if (selectorSelected === id) {
      setVisible(!visible);
    } else {
      setSelectorSelected(id);
      setVisible(true);
    }
  },
    [
      id,
      selectorSelected,
      setVisible,
      setSelectorSelected,
      visible,
    ]);
  const title = props.title ?? `${label} Sidebar`;
  const component =
    icon
      ? <Icon size={20} className="sidebar-selector-icon" id={icon} />
      : <label className="sidebar-selector-label">{label}</label>;
  return (
    <div className={className} title={title} onClick={onClick}>
      {component}
    </div>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sidebar Toggle Selector                                            --- */
/* -------------------------------------------------------------------------- */

/**
   Dedicated selector-like control used to collapse or expand the sidebar
   panel without changing the currently selected sidebar selector.
 */
function ToggleSelector(props: SidebarPanel): JSX.Element {
  const { visible, setVisible } = props;
  const className = classes(
    'sidebar-selector',
    'sidebar-selector-toggle',
    'dome-color-frame',
    visible && 'sidebar-selector-selected',
  );
  const title = `${visible ? 'Collapse' : 'Expand'} sidebar`;
  const id = visible ? 'ANGLE.LEFT' : 'ANGLE.RIGHT';
  return (
    <div
      className={className}
      title={title}
      onClick={() => setVisible(!visible)}
    >
      <Icon size={20} className="sidebar-selector-icon" id={id} />
    </div>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sidebar Main Components                                            --- */
/* -------------------------------------------------------------------------- */

export function Selectors(props: SidebarState): JSX.Element {
  const { selection, panel } = props;
  const selectors = selection.registeredSidebars.map((sb) => (
    <Selector
      key={sb.id}
      panel={panel}
      selection={selection}
      {...sb} />
  ));
  const selectorsClasses = classes(
    selection.registeredSidebars.length <= 1 && 'dome-erased',
  );
  const itemsClassName = classes(
    'sidebar-items',
    'dome-color-frame',
    !panel.visible && 'sidebar-items-collapsed',
  );

  return (
    <div className={itemsClassName}>
      <div className={selectorsClasses}>{selectors}</div>
      <ToggleSelector {...panel} />
    </div>
  );
}

export function Panels(props: SidebarSelectionState): JSX.Element {
  const { selectorSelected, registeredSidebars } = props;
  const sidebars = registeredSidebars.map((sb) => (
    <SideBar
      key={sb.id}
      className={selectorSelected === sb.id ? '' : 'dome-erased'}
    >
      <div className="sidebar-ruler" />
      <Catch label={sb.id}>
        {sb.children}
      </Catch>
    </SideBar>
  ));

  return <div className="sidebar-view">{sidebars}</div>;
}

// --------------------------------------------------------------------------
