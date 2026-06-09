/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Table of (multiple) locations
// --------------------------------------------------------------------------

import React from 'react';
import { GlobalState, useGlobalState } from 'dome/data/states';
import * as States from 'frama-c/states';

import { CompactModel } from 'dome/table/arrays';
import { Table, Column, Renderer } from 'dome/table/views';
import { Label, Cell } from 'dome/controls/labels';
import {
  IconButton, Multiselect, MultiselectItem
} from 'dome/controls/buttons';
import { Inset } from 'dome/frame/toolbars';
import { Dropdown } from 'dome/dialogs';
import { TitleBar } from 'ivette';
import * as Display from 'ivette/display';
import * as Ast from 'frama-c/kernel/api/ast';
import * as Server from 'frama-c/server';
import { current as currentProject } from 'frama-c/kernel/api/project';

// --------------------------------------------------------------------------
// --- Global Multi-Selection
// --------------------------------------------------------------------------

export interface MultiSelection {
  plugin: string;
  label: string;
  title?: string;
  markers: Ast.marker[];
  scopes?: Ast.decl[];
  index?: number;
}

const emptySelection = {
  plugin: '', label: '', title: '', markers: [], index: 0
};
const MultiSelection = new GlobalState<MultiSelection>(emptySelection);

export function useSelection(): MultiSelection {
  const [s] = useGlobalState(MultiSelection);
  const [scopes, setScopes] = React.useState<Ast.decl[]>([]);
  const getAttr = States.useSyncArrayGetter(Ast.markerAttributes);

  React.useEffect(() => {
    const newScopes = new Set<Ast.decl>();
    s.markers.forEach(marker => {
      const scope = getAttr(marker)?.scope;
      if(scope) newScopes.add(scope);
    });
    setScopes([...newScopes]);
  }, [s, setScopes, getAttr]);

  return { ...s, scopes };
}

function updateSelection(s: MultiSelection): void {
  MultiSelection.setValue(s);
  const marker = s.index !== undefined ? s.markers[s.index] : undefined;
  if (marker) States.setSelected(marker);
}

export function setSelection(s: MultiSelection): void {
  updateSelection(s);
  if (s.plugin && s.markers.length > 0) {
    const label = `${s.plugin}: ${s.markers.length} locations selected`;
    const title =
      `${s.label}: ${s.markers.length} locations selected`
      + `\nListed in the 'Locations' panel`;
    Display.showMessage({ label, title });
    Display.alertComponent('fc.kernel.locations');
  }
}

export function setIndex(index: number): void {
  const s = MultiSelection.getValue();
  updateSelection({ ...s, index });
}

function sameMarkers(xs: Ast.marker[], ys: Ast.marker[]): boolean {
  if (xs.length !== ys.length) return false;
  for (let k = 0; k < xs.length; k++)
    if (xs[k] !== ys[k]) return false;
  return true;
}

function sameSelection(u: MultiSelection, v: MultiSelection): boolean {
  if (u.label !== v.label) return false;
  if (u.title !== v.title) return false;
  return sameMarkers(u.markers, v.markers);
}

/**
   Update the list of markers and select its first element,
   or cycle to the next element wrt current selection.
 */
export function setNextSelection(s: MultiSelection): void {
  const selection = MultiSelection.getValue();
  if (s.index === undefined && sameSelection(selection, s)) {
    const { index, markers } = selection;
    const target = index === undefined ? 0 : index + 1;
    const select = target < markers.length ? target : 0;
    updateSelection({ ...selection, index: select });
  } else {
    updateSelection(s);
  }
}

export function clearSelection(): void {
  MultiSelection.setValue(emptySelection);
}

function gotoIndex(index: number): void {
  const selection = MultiSelection.getValue();
  if (0 <= index && index <= selection.markers.length)
    updateSelection({ ...selection, index });
}

function goToNextVisibleIndex(currentIndex: number, model: Model): void {
  const selection = MultiSelection.getValue();
  const nextIndex = currentIndex + 1;
  if (nextIndex >= selection.markers.length) return;
  const iTab = model.getIndexOf(selection.markers[nextIndex]);
  if(iTab !== undefined) updateSelection({ ...selection, index: nextIndex });
  else return goToNextVisibleIndex(nextIndex, model);
}

function goToPrevVisibleIndex(currentIndex: number, model: Model): void {
  const selection = MultiSelection.getValue();
  const prevIndex = currentIndex - 1;
  if (prevIndex < 0) return;
  const iTab = model.getIndexOf(selection.markers[prevIndex]);
  if(iTab !== undefined) updateSelection({ ...selection, index: prevIndex });
  else return goToPrevVisibleIndex(prevIndex, model);
}

{
  Server.onReady(clearSelection);
  Server.onSignal(currentProject.signal, clearSelection);
  Server.onShutdown(clearSelection);
}

// --------------------------------------------------------------------------
// --- Locations Panel
// --------------------------------------------------------------------------

interface Data {
  index: number,
  attr: Ast.markerAttributesData,
  decl: Ast.declAttributesData,
}

class Model extends CompactModel<Ast.marker, Data> {
  constructor() { super(({ attr }) => attr.marker); }
}

const renderIndex: Renderer<number> =
  (index) => <Cell label={`${index + 1}`} />;

const renderDecl: Renderer<Data> =
  (d) => {
    const name = d.decl.name;
    const label = d.decl.label;
    return <Cell label={name} title={label} />;
  };

const renderLocation: Renderer<Data> =
  (d) => {
    const loc = d.attr.sloc;
    if (loc)
      return <Cell label={`${loc.base}:${loc.line}`} title={loc.file} />;
    else
      return null;
  };

const renderAttr: Renderer<Ast.markerAttributesData> =
  (attr) => <Cell title={attr.descr}>{attr.descr}</Cell>;

export default function LocationsTable(): JSX.Element {

  // Hooks
  const model = React.useMemo(() => new Model(), []);
  const getDecl = States.useSyncArrayGetter(Ast.declAttributes);
  const getAttr = States.useSyncArrayGetter(Ast.markerAttributes);
  const { label, title, markers, index, scopes } = useSelection();
  const previousScopesRef = React.useRef< Ast.decl[]>([]);
  React.useEffect(() => {
    model.replaceAllDataWith(
      markers.map((marker, index): Data => {
        const attr = getAttr(marker) ?? Ast.markerAttributesDataDefault;
        const decl = getDecl(attr.scope) ?? Ast.declAttributesDataDefault;
        return { index, attr, decl };
      })
    );
  }, [model, markers, getAttr, getDecl]);
  const selected = index !== undefined ? markers[index] : undefined;
  const size = markers.length;
  const kindex = index === undefined ? (-1) : index;
  const indexLabel = index === undefined ? '…' : index + 1;
  const positionLabel = `${indexLabel} / ${size}`;

  /** filter */
  const [visibleScopes, setVisibleScopes] =
    React.useState<Set<string>>(new Set(scopes));

  React.useEffect(() => {
    const previousScopes = previousScopesRef.current;
    const isScopesChanged =
      previousScopes.length !== scopes?.length
      || !previousScopes.every(e => scopes.includes(e));
    if(isScopesChanged) {
      setVisibleScopes(new Set(scopes));
      previousScopesRef.current = scopes ?? [];
    }
  }, [scopes]);

  const setVisible = React.useCallback((a: Ast.decl) => {
    if(visibleScopes.has(a)) visibleScopes.delete(a);
    else visibleScopes.add(a);
    setVisibleScopes(new Set(visibleScopes));
  }, [visibleScopes, setVisibleScopes]);

  React.useEffect(() => {
    model.setFilter(({ decl }) => visibleScopes.has(decl.decl));
  }, [model, visibleScopes]);

  const itemsComp = scopes && scopes.map((e, i) =>
    <MultiselectItem key={i} item={{
       label: getDecl(e)?.name || e,
       id: e,
       enabled: true,
       checked: visibleScopes.has(e),
       onClick: () => setVisible(e)
      }}
    />
  );

  const allChecked = visibleScopes.size === scopes?.length;
  const checkAllItem =
    { label: allChecked ? 'Uncheck all' : 'Check all',
      enabled: true,
      checked: false,
      onClick: () => setVisibleScopes(new Set(allChecked ? [] : scopes)),
    };

  const filter =
    <Multiselect>
      <MultiselectItem key={'all'} item={checkAllItem} />
      <MultiselectItem key={'separator'} item='separator' />
      {itemsComp && itemsComp}
    </Multiselect>;

  const filterKind =
    visibleScopes.size === scopes?.length ? 'positive' :
    visibleScopes.size === 0 ? 'negative' : 'warning';

  const filterEnabled = scopes && scopes.length > 1;

  const filterButton =
    <IconButton
      icon='FILTER' kind={filterKind} enabled={filterEnabled}
      title='Filtering options'
    />;

  // Component
  return (
    <>
      <TitleBar help="framac-locations">
        <IconButton
          icon='ANGLE.LEFT'
          title='Previous location'
          enabled={0 < kindex}
          onClick={() => goToPrevVisibleIndex(kindex, model)}
        />
        <IconButton
          icon='ANGLE.RIGHT'
          title='Next location'
          enabled={(-1) <= kindex && kindex + 1 < size}
          onClick={() => goToNextVisibleIndex(kindex, model)}
        />
        <Inset />
        <Label
          className='component-info'
          display={0 < size}
          label={positionLabel}
          title='Current location index / Number of locations' />
        <Inset />
        <Dropdown control={filterButton}>
          {filter}
        </Dropdown>
        <Inset />
        <IconButton
          icon='TRASH'
          title='Cancel selected locations'
          onClick={clearSelection}
        />
        <Inset />
      </TitleBar>
      <Label className='locations' label={label} title={title} />
      <Table
        model={model}
        display={size > 0}
        selection={selected}
        onSelection={(row) => gotoIndex(row.index) }
        settings="ivette.locations.table"
      >
        <Column
          id='index' label='#' align='center' width={25}
          render={renderIndex} />
        <Column
          id='decl' label='Scope'
          width={100}
          getter={(d: Data) => d}
          render={renderDecl} />
        <Column
          id='location' label='Location'
          width={180}
          getter={(d: Data) => d}
          render={renderLocation} />
        <Column
          id='attr' label='Marker' fill
          render={renderAttr} />
      </Table>
    </>
  );
}

// --------------------------------------------------------------------------
