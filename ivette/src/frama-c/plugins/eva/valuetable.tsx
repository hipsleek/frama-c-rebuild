/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import _ from 'lodash';

import * as Ivette from 'ivette';
import * as Display from 'ivette/display';

import * as Dome from 'dome';
import { GlobalState, useGlobalState } from 'dome/data/states';
import { classes } from 'dome/misc/utils';
import { Icon } from 'dome/controls/icons';
import { Button, Inset } from 'dome/frame/toolbars';
import { Cell, Code, Label } from 'dome/controls/labels';
import { IconButton } from 'dome/controls/buttons';
import { Filler, Hpack, Hfill, Vpack, Vfill, Hbox } from 'dome/layout/boxes';
import { Modal, showModal, closeModal } from 'dome/dialogs';
import { FieldState, TextField, useState } from 'dome/layout/forms';

import * as States from 'frama-c/states';
import * as Server from 'frama-c/server';
import * as Ast from 'frama-c/kernel/api/ast';
import * as ASTview from 'frama-c/kernel/ASTview';
import * as Values from 'frama-c/plugins/eva/api/values';
import { current as currentProject } from 'frama-c/kernel/api/project';
import { MarkerText, Modifier, selectMarker, textToString }
  from 'frama-c/richtext';

import { EvaReady, EvaStatus } from './components/AnalysisStatus';
import { evaComputationValue } from './components/Tools';

/* -------------------------------------------------------------------------- */
/* --- Miscellaneous definitions                                          --- */
/* -------------------------------------------------------------------------- */

type Request<A, B> = (a: A) => Promise<B>;

type Alarm = ['True' | 'False' | 'Unknown', string]
function getAlarmStatus(alarms: Alarm[] | undefined): string {
  if (!alarms) return 'none';
  if (alarms.length === 0) return 'none';
  if (alarms.find(([st, _]) => st === 'False')) return 'False';
  else return 'Unknown';
}

type MarkerTracked = ['Tracked', boolean]
type MarkerPinned = ['Pinned', boolean]
type MarkerStatus = MarkerTracked | MarkerPinned | 'JustFocused'

function MarkerStatusClass(status: MarkerStatus): string {
  if (status === 'JustFocused') return 'eva-header-just-focused';
  const [kind, focused] = status;
  return 'eva-header-' + kind.toLowerCase() + (focused ? '-focused' : '');
}

function isPinnedMarker(status: MarkerStatus): boolean {
  if (status === 'JustFocused') return false;
  const [kind] = status;
  return kind === 'Pinned';
}

interface TableCellProps {
  children?: JSX.Element | JSX.Element[];
  right?: JSX.Element;
  align?: 'left' | 'center';
}

function TableCell(props: TableCellProps): JSX.Element {
  const { children, right, align = 'center' } = props;
  const leftVisible = align === 'center' ? 'block' : 'none';
  return (
    <div className='eva-cell-container'>
      <div className='eva-cell-left' style={{ display: leftVisible }} />
      <div className='eva-cell-content'>
        {children}
      </div>
      <div className='eva-cell-right'>
        {right}
      </div>
    </div>
  );
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Callstack related definitions                                      --- */
/* -------------------------------------------------------------------------- */

/* Callstacks are declared by the server. We add the `Summary` construction to
 * cleanly represent the summary of all the callstacks. */
type callstack = 'Summary' | Values.callstack

/* `getCallstacks` request */
function useCallstacks(): Request<Ast.marker[], callstack[]> {
  return React.useCallback((m) => Server.send(Values.getCallstacks, m), []);
}

/* `getCallstackInfo` request */
function useCallsites(): Request<callstack, Values.callsite[]> {
  return React.useCallback(
    (c: callstack): Promise<Values.callsite[]> => {
      if (c !== 'Summary') return Server.send(Values.getCallstackInfo, c);
      else return Promise.resolve([]);
    }, []);
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Probe related definitions                                          --- */
/* -------------------------------------------------------------------------- */

/* An Evaluation keeps track of the values at relevant control point around a
 * statement, along with the potential errors */
interface Evaluation {
  errors?: string;
  vBefore?: Values.evaluation;
  vAfter?: Values.evaluation;
  vThen?: Values.evaluation;
  vElse?: Values.evaluation;
}

/* A Probe is a location along with data representing textually what it is, the
 * considered statement, if it is an effectfull one or one with conditions.
 * Moreover, it gives a function that computes an Evaluation for a given
 * callstack. This computation is asynchronous. */
interface Probe {
  marker: Ast.marker;
  scope: Ast.decl;
  stmt?: Ast.marker;
  code?: string;
  evaluable: boolean;
  effects?: boolean;
  condition?: boolean;
  evaluate: Request<callstack, Evaluation>
}

/* Builds a Probe given a Location */
function useProbe(): Request<[Ast.decl, Ast.marker], Probe> {
  const getValues = React.useCallback(
    ([target, cs]: [Ast.marker, callstack]): Promise<Evaluation> => {
      const callstack = cs === 'Summary' ? undefined : cs;
      return Server.send(Values.getValues, { target, callstack });
    }, []
  );
  return React.useCallback(
    async ([scope, marker]: [Ast.decl, Ast.marker]): Promise<Probe> => {
      const infos = await Server.send(Values.getProbeInfo, marker);
      const evaluate: Request<callstack, Evaluation> = (c) =>
        getValues([marker, c]);
      return { marker, scope, ...infos, evaluate };
    }, [getValues]
  );
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Statement Component                                                --- */
/* -------------------------------------------------------------------------- */

interface StmtProps {
  stmt?: Ast.marker;
  marker?: Ast.marker;
  short?: boolean;
}

function Stmt(props: StmtProps): JSX.Element | null {
  const { stmt, marker, short } = props;
  const { descr, scope } = States.useMarker(stmt);
  const { sloc } = States.useMarker(marker);
  const { name: fct } = States.useDeclaration(scope);
  if (!marker || !fct) return null;
  // Location sloc should always be defined for statements.
  const label = short ? `@L${sloc?.line}` : `@${sloc?.base}:${sloc?.line}`;
  const title = stmt ? descr : "Start of function " + fct;
  const className = 'dome-text-cell eva-stmt';
  return <span className={className} title={title}>{label}</span>;
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Information on the alarms in a given callstack                     --- */
/* -------------------------------------------------------------------------- */

function AlarmsInfos(probe?: Probe): Request<callstack, JSX.Element> {
  return async (c: callstack): Promise<JSX.Element> => {
    const evaluation = await probe?.evaluate(c);
    const alarms = evaluation?.vBefore?.alarms ?? [];
    if (alarms.length <= 0) return <></>;
    const renderAlarm = ([status, alarm]: Alarm): JSX.Element => {
      const className = classes('eva-alarm-info', `eva-alarm-${status}`);
      return <Code className={className} icon="WARNING">{alarm}</Code>;
    };
    const children = React.Children.toArray(alarms.map(renderAlarm));
    return <Vpack className="eva-info">{children}</Vpack>;
  };
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Information on the selected callstack                              --- */
/* -------------------------------------------------------------------------- */

interface StackInfosProps {
  callsites: Values.callsite[];
  isSelected: boolean;
  close: () => void;
}

async function StackInfos(props: StackInfosProps): Promise<JSX.Element> {
  const { callsites, isSelected, close } = props;
  const selectedClass = isSelected ? 'eva-focused' : '';
  const className = classes('eva-callsite', selectedClass);
  if (callsites.length <= 1) return <></>;
  const makeCallsite = ({ caller, stmt }: Values.callsite): JSX.Element => {
    if (!caller || !stmt) return <></>;
    const { name } = States.getDeclaration(caller);
    const key = `${caller}@${stmt}`;
    const select = (meta: boolean): void => {
      States.setSelected(stmt, meta);
    };
    const onClick = (evt: React.MouseEvent): void => { select(evt.altKey); };
    const onDoubleClick = (evt: React.MouseEvent): void => {
      evt.preventDefault();
      select(true);
    };
    return (
      <Cell
        key={key}
        icon='TRIANGLE.LEFT'
        className={className}
        onClick={onClick}
        onDoubleClick={onDoubleClick}
      >
        {name}
        <Stmt stmt={stmt} marker={stmt} />
      </Cell>
    );
  };
  const children = React.Children.toArray(callsites.map(makeCallsite));
  return (
    <div className='eva-info'>
      <Hpack className='eva-info-wrap'>{children}</Hpack>
      <Hfill />
      <IconButton
        icon='CROSS'
        className='eva-button'
        onClick={close}
      />
    </div>
  );
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Probe Header Component                                             --- */
/* -------------------------------------------------------------------------- */
/* --- Header of a column, describing the evaluated expression and its    --- */
/* --- status inside the component (pinned, tracked, etc).                --- */
/* -------------------------------------------------------------------------- */

interface ProbeHeaderProps {
  probe: Probe;
  status: MarkerStatus;
  pinProbe: (pin: boolean) => void;
  selectProbe: () => void;
  removeProbe: () => void;
  locEvt: Dome.Event<Ast.marker>;
}

function ProbeHeader(props: ProbeHeaderProps): JSX.Element {
  const { probe, status, locEvt } = props;
  const { code = '(error)', stmt, marker } = probe;
  const color = classes(MarkerStatusClass(status), 'eva-table-header-sticky');
  const { selectProbe, removeProbe, pinProbe } = props;
  const span = 1 + (probe.effects ? 1 : 0) + (probe.condition ? 2 : 0);
  const buttonClass = classes('eva-button', 'eva-header-button');

  // When the location is selected, we scroll the header into view, making it
  // appears wherever it was.
  const ref = React.createRef<HTMLTableCellElement>();
  locEvt.on((l) => { if (l === probe.marker) ref.current?.scrollIntoView(); });

  const isPinned = isPinnedMarker(status);
  const pinText = isPinned ? 'Unpin' : 'Pin';
  const onClick = (): void => {
    States.setSelected(marker);
    selectProbe();
  };
  const onDoubleClick = (): void => pinProbe(!isPinned);
  const onContextMenu = (): void => {
    const items: Dome.PopupMenuItem[] = [];
    const pinLabel = `${pinText} column for ${code}`;
    items.push({ label: pinLabel, onClick: onDoubleClick });
    const removeLabel = `Remove column for ${code}`;
    items.push({ label: removeLabel, onClick: removeProbe });
    Dome.popupMenu(items);
  };

  const buttons =
    <div>
      <IconButton
        icon='PIN'
        className={buttonClass}
        title={`${pinText} the column`}
        selected={isPinned}
        onClick={onDoubleClick}
      />
      <IconButton
        icon="CIRC.CLOSE"
        className={buttonClass}
        title="Remove the column"
        onClick={() => removeProbe()}
      />
    </div>;

  return (
    <th
      ref={ref}
      className={color}
      colSpan={span}
      onClick={onClick}
      onDoubleClick={onDoubleClick}
      onContextMenu={onContextMenu}
    >
      <TableCell right={buttons}>
        <div className='eva-header-text-overflow'>
          <span className='dome-text-cell' title={code}>{code}</span>
        </div>
        <Stmt stmt={stmt} marker={marker} short={true} />
      </TableCell>
    </th>
  );
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Probe Description Component                                        --- */
/* -------------------------------------------------------------------------- */
/* --- Description of a table column, i.e. if it contains values          --- */
/* --- evaluated before or after the considered statement.                --- */
/* -------------------------------------------------------------------------- */

interface ProbeDescrProps {
  probe: Probe;
}

function ProbeDescr(props: ProbeDescrProps): JSX.Element[] {
  const { probe } = props;
  const { name, kind } = States.getDeclaration(probe.scope);
  const valuesClass = classes('eva-table-values', 'eva-table-values-center');
  const tableClass = classes('eva-table-descrs', 'eva-table-descr-sticky');
  const cls = classes(valuesClass, tableClass);
  const elements: JSX.Element[] = [];
  function push(title: string, children: JSX.Element | string): void {
    elements.push(<td className={cls} title={title}>{children}</td>);
  }
  if (!probe.effects && !probe.condition) {
    if (probe.stmt)
      push('Values at the statement', '-');
    else if (kind === 'FUNCTION')
      push('Values at the start of function ' + name, '-');
    else
      push('Values at the start of the analysis', '-');
  }
  if (probe.effects || probe.condition)
    push('Values just before the statement', 'Before');
  if (probe.effects)
    push('Values just after the statement', 'After');
  if (probe.condition) {
    const pushCondition = (s: string): void => {
      const t = `Values after the condition, in the ${s.toLowerCase()} branch`;
      const child =
        <div className='eva-header-after-condition'>
          After
          <div className='eva-stmt'>{`(${s})`}</div>
        </div>;
      push(t, child);
    };
    pushCondition('Then');
    pushCondition('Else');
  }
  return elements;
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Probe Values Component                                             --- */
/* -------------------------------------------------------------------------- */
/* --- This component represents the contents of one of the table that    --- */
/* --- displays values information. As the content depends on the         --- */
/* --- considered callstack, we decided to return a function that build   --- */
/* --- the actual component when given a callstack. It avoids useless     --- */
/* --- computational boilerplate.                                         --- */
/* -------------------------------------------------------------------------- */

interface ProbeValuesProps {
  probe: Probe;
  pinProbe: (loc: Ast.marker, pin: boolean) => void;
  isSelectedCallstack: (c: callstack) => boolean;
}

function ProbeValues(props: ProbeValuesProps): Request<callstack, JSX.Element> {
  const { probe, pinProbe, isSelectedCallstack } = props;

  const onSelected = (marker: Ast.marker, modifier: Modifier): void => {
    /* Pin the current probe so that its column is not removed
       when the selected marker changes. */
    pinProbe(probe.marker, true);
    selectMarker(marker, modifier);
  };

  // Building common parts
  const onContextMenu = (evaluation?: Values.evaluation) => (): void => {
    const { value = '', pointedVars = [] } = evaluation ?? {};
    const items: Dome.PopupMenuItem[] = [];
    const copy = (): void => {
      if (value) navigator.clipboard.writeText(textToString(value));
    };
    if (value !== '') items.push({ label: 'Copy to clipboard', onClick: copy });
    if (items.length > 0 && pointedVars.length > 0) items.push('separator');
    pointedVars.forEach((lval) => {
      const [text, lvalMarker] = lval;
      const label = `Display values for ${text}`;
      const onItemClick = (): void => onSelected(lvalMarker, 'NORMAL');
      items.push({ label, onClick: onItemClick });
    });
    if (items.length > 0) Dome.popupMenu(items);
  };

  return async (callstack: callstack): Promise<JSX.Element> => {
    const evaluation = await probe.evaluate(callstack);
    const { vBefore, vAfter, vThen, vElse } = evaluation;
    const isSelected = isSelectedCallstack(callstack);
    const selected = isSelected && callstack !== 'Summary' ? 'eva-focused' : '';
    const font = callstack === 'Summary' ? 'eva-italic' : '';
    const c = classes('eva-table-values', selected, font);
    const kind = callstack === 'Summary' ? 'one' : 'this';
    const title = `At least one alarm is raised in ${kind} callstack`;
    function td(e?: Values.evaluation, colSpan = 1): JSX.Element {
      const { alarms, value = '-' } = e ?? {};
      const status = getAlarmStatus(alarms);
      const alarmClass = classes('eva-cell-alarms', `eva-alarm-${status}`);
      const align = value?.includes('\n') ? 'left' : 'center';
      const warning =
        <Icon className={alarmClass} size={10} title={title} id="WARNING" />;
      return (
        <td className={c} colSpan={colSpan} onContextMenu={onContextMenu(e)}>
          <TableCell right={warning} align={align}>
            <MarkerText onSelected={onSelected} text={value} />
          </TableCell>
        </td>
      );
    }
    const elements: JSX.Element[] = [];
    if (probe.effects && _.isEqual(vBefore, vAfter))
      elements.push(td(vBefore, 2));
    else {
      if (!probe.effects && !probe.condition)
        elements.push(td(vBefore));
      if (probe.effects || probe.condition)
        elements.push(td(vBefore));
      if (probe.effects)
        elements.push(td(vAfter));
      if (probe.condition)
        elements.push(td(vThen), td(vElse));
    }
    return <>{React.Children.toArray(elements)}</>;
  };
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Row header describing the corresponding callstack                  --- */
/* -------------------------------------------------------------------------- */

interface CallsiteCellProps {
  callstack: callstack | 'None' | 'Header';
  index?: number;
  getCallsites: Request<callstack, Values.callsite[]>;
  selectedClass?: string;
}

function makeStackTitle(calls: Values.callsite[]): string {
  const cs = calls.slice(1);
  if (cs.length > 0)
    return `Callstack: ${cs.map((c) => c.callee).join(' \u2190 ')}`;
  return 'Callstack Details';
}

async function CallsiteCell(props: CallsiteCellProps): Promise<JSX.Element> {
  const { callstack, index, getCallsites, selectedClass = '' } = props;
  const baseClasses = classes('eva-table-callsite-box', selectedClass);
  const cls = classes(baseClasses, 'eva-table-value-sticky');
  switch (callstack) {
    case 'Header': {
      const headerCls = classes(baseClasses, 'eva-table-header-sticky');
      const title = 'Callstack at which expressions are evaluated';
      return <td className={headerCls} rowSpan={2} title={title}>{'#'}</td>;
    }
    case 'None': {
      const text = '-';
      const title = 'Global evaluation';
      return <td className={cls} title={title}>{text}</td>;
    }
    case 'Summary': {
      const text = '∑';
      const title = 'Summary: value consolidated across all callstacks';
      return <td className={cls} title={title}>{text}</td>;
    }
    default: {
      const callsites = await getCallsites(callstack);
      const title = makeStackTitle(callsites);
      const text = index ? index.toString() : '0';
      return <td className={cls} title={title}>{text}</td>;
    }
  }
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Scope Section Component                                            --- */
/* -------------------------------------------------------------------------- */

interface ScopeProps {
  scope: Ast.decl;
  markers: Map<Ast.marker, MarkerStatus>;
  close: () => void;
  getProbe: Request<[Ast.decl, Ast.marker], Probe>;
  pinProbe: (marker: Ast.marker, pin: boolean) => void;
  selectProbe: (probe: Probe) => void;
  removeProbe: (probe: Probe) => void;
  folded: boolean;
  setFolded: (folded: boolean) => void;
  getCallsites: Request<callstack, Values.callsite[]>;
  byCallstacks: boolean;
  getCallstacks: Request<Ast.marker[], callstack[]>;
  setByCallstacks: (byCallstack: boolean) => void;
  selectCallstack: (callstack: callstack) => void;
  isSelectedCallstack: (c: callstack) => boolean;
  locEvt: Dome.Event<Ast.marker>;
  startingCallstack: number;
  changeStartingCallstack: (n: number) => void;
}

const PageSize = 99;

async function ScopeSection(props: ScopeProps): Promise<JSX.Element> {
  const {
    scope, folded, isSelectedCallstack, locEvt,
    byCallstacks, getCallsites,
    getCallstacks: getCS, pinProbe,
    setFolded, setByCallstacks, close,
    startingCallstack,
    changeStartingCallstack,
  } = props;
  const { name } = States.getDeclaration(scope);
  const displayTable = folded ? 'none' : 'block';
  type RowHandler = React.MouseEventHandler<HTMLTableRowElement>;
  const onClick: (c: callstack) => RowHandler = (c) => (event) => {
    const elt = document.elementFromPoint(event.clientX, event.clientY);
    if (elt?.localName !== 'span')
      props.selectCallstack(isSelectedCallstack(c) ? 'Summary' : c);
  };

  /* Computes the relevant callstacks */
  const markers = Array.from(props.markers.keys());
  const allCallstacks = await getCS(markers);
  const onlyOneCallstack = allCallstacks.length === 1;
  const callstacks = byCallstacks || onlyOneCallstack ? allCallstacks : [];

  /* Computes the relevant data for each marker */
  interface Data { probe: Probe; summary: Evaluation; status: MarkerStatus }
  const entries = Array.from(props.markers.entries());
  const data = await Promise.all(entries.map(async ([marker, status]) => {
    const probe = await props.getProbe([scope, marker]);
    const summary = await probe.evaluate('Summary');
    return { probe, summary, status };
  }));
  const doCall = data.length > 0;

  /* Computes the headers for each marker */
  const headerCall = await CallsiteCell({ getCallsites, callstack: 'Header' });
  const headers = await Promise.all(data.map((d: Data) => {
    const { probe } = d;
    const { marker } = probe;
    const pinProbe = (pin: boolean): void => props.pinProbe(marker, pin);
    const selectProbe = (): void => props.selectProbe(probe);
    const removeProbe = (): void => props.removeProbe(probe);
    const scopes = { selectProbe, pinProbe, removeProbe };
    return ProbeHeader({ ...d, ...scopes, locEvt });
  }));

  /* Computes the columns descriptions */
  const descrs = data.map((d) => ProbeDescr(d)).flat();

  /* Computes the summary values */
  const miscs = { pinProbe, isSelectedCallstack };
  const builders = data.map((d: Data) => ProbeValues({ ...d, ...miscs }));
  const summary = await Promise.all(builders.map((b) => b('Summary')));
  const summaryKind = allCallstacks.length === 0 ? 'None' : 'Summary';
  const summCall = await CallsiteCell({ callstack: summaryKind, getCallsites });
  let summaryRow = <></>;
  if (!onlyOneCallstack) {
    summaryRow =
      <tr key={'Summary'} onClick={onClick('Summary')}>
        {doCall ? summCall : undefined}
        {React.Children.toArray(summary)}
      </tr>;
  }

  /* Computes the values for each callstack */
  const start = Math.max(1, startingCallstack);
  const stop = Math.min(start + PageSize, callstacks.length);
  const values = await Promise.all(callstacks.map(async (callstack, n) => {
    const index = n + 1;
    if (start > index || stop < index) return <></>;
    const selectedClass = isSelectedCallstack(callstack) ? 'eva-focused' : '';
    const callProps = { selectedClass, getCallsites };
    const call = await CallsiteCell({ index, callstack, ...callProps });
    const values = await Promise.all(builders.map((b) => b(callstack)));
    return (
      <tr key={callstack} onClick={onClick(callstack)}>
        {call}
        {React.Children.toArray(values)}
      </tr>
    );
  }));

  /* We change the starting callstack dynamically when we reach the ends of the
   * scroll to avoid to build the complete table */
  const onScroll: React.UIEventHandler<HTMLDivElement> = (event) => {
    const { scrollTop, scrollHeight, clientHeight } = event.currentTarget;
    if (scrollTop / (scrollHeight - clientHeight) <= 0.1)
      changeStartingCallstack(Math.max(startingCallstack - 10, 0));
    const botGap = (scrollHeight - scrollTop - clientHeight) / scrollHeight;
    const lastCallstack = startingCallstack + PageSize;
    if (botGap <= 0.1 && lastCallstack !== callstacks.length) {
      const maxStart = callstacks.length - PageSize;
      const start = Math.min(startingCallstack + 10, maxStart);
      changeStartingCallstack(start);
    }
  };

  /* Builds the component */
  return (
    <>
      <Hpack className="eva-function">
        <IconButton
          className="eva-fct-fold"
          icon={folded ? 'ANGLE.RIGHT' : 'ANGLE.DOWN'}
          onClick={() => setFolded(!folded)}
        />
        <Cell className="eva-fct-name">{name}</Cell>
        <Filler />
        <div className='eva-nb-callstacks'>
          {`${allCallstacks.length} callstack${onlyOneCallstack ? '' : 's'}`}
        </div>
        <IconButton
          icon="ITEMS.LIST"
          className="eva-button"
          selected={byCallstacks}
          enabled={allCallstacks.length > 1}
          title="Show values by callstack"
          onClick={() => setByCallstacks(!byCallstacks)}
        />
        <Inset />
        <IconButton
          icon="CROSS"
          className="eva-button"
          title="Close"
          onClick={close}
        />
      </Hpack>
      <div
        onScroll={onScroll}
        className='eva-table-container'
        style={{ display: displayTable }}
      >
        <table className='eva-table'>
          <tbody>
            <tr>
              {doCall ? headerCall : undefined}
              {React.Children.toArray(headers)}
            </tr>
            <tr>
              {React.Children.toArray(descrs)}
            </tr>
            {summaryRow}
            {React.Children.toArray(values)}
          </tbody>
        </table>
      </div>
    </>
  );
}

/* -------------------------------------------------------------------------- */



/* -------------------------------------------------------------------------- */
/* --- Scope Manager                                                      --- */
/* -------------------------------------------------------------------------- */
/* --- The Scope Manager is responsible of all the data related to        --- */
/* --- programs scopes (mainly functions).                                --- */
/* -------------------------------------------------------------------------- */

/* Information on one scope */
class ScopeInfos {
  readonly scope: Ast.decl;                 // Scope decl
  readonly pinned = new Set<Ast.marker>();  // Pinned markers
  readonly tracked = new Set<Ast.marker>(); // Tracked markers
  startingCallstack = 1;                    // First displayed callstack
  byCallstacks: boolean;                    // True if displayed by callstacks
  folded = false;                           // True if folded

  constructor(scope: Ast.decl, byCallstacks: boolean) {
    this.scope = scope;
    this.byCallstacks = byCallstacks;
  }

  has(marker: Ast.marker): boolean {
    const pinned = this.pinned.has(marker);
    const tracked = this.tracked.has(marker);
    return pinned || tracked;
  }

  pin(marker: Ast.marker): void {
    this.pinned.add(marker);
    this.tracked.delete(marker);
  }

  delete(marker: Ast.marker): void {
    this.pinned.delete(marker);
    this.tracked.delete(marker);
  }

  isEmpty(): boolean {
    return this.pinned.size === 0 && this.tracked.size === 0;
  }

  markers(probe?: Probe): Map<Ast.marker, MarkerStatus> {
    const inScope = probe?.scope === this.scope;
    const ms = new Map<Ast.marker, MarkerStatus>();
    const p0 = probe?.marker;
    this.pinned.forEach((p) => ms.set(p, ['Pinned', inScope && p0 === p]));
    this.tracked.forEach((p) => ms.set(p, ['Tracked', inScope && p0 === p]));
    if (inScope && p0 && !this.has(p0)) ms.set(p0, 'JustFocused');
    return new Map(Array.from(ms.entries()).reverse());
  }

}

/* State keeping tracks of information for every relevant scopes */
class ScopesManager {

  private readonly cache = new Map<Ast.decl, ScopeInfos>();
  private byCallstacks = false;

  constructor() {
    this.newScope = this.newScope.bind(this);
    this.getInfos = this.getInfos.bind(this);
    this.setByCallstacks = this.setByCallstacks.bind(this);
    this.setFolded = this.setFolded.bind(this);
    this.pin = this.pin.bind(this);
    this.removeLocation = this.removeLocation.bind(this);
    this.delete = this.delete.bind(this);
    this.clear = this.clear.bind(this);
    this.map = this.map.bind(this);
  }

  newScope(scope: Ast.decl): void {
    if (!this.cache.has(scope))
      this.cache.set(scope, new ScopeInfos(scope, this.byCallstacks));
  }

  private getInfos(scope: Ast.decl): ScopeInfos {
    const { cache } = this;
    let infos = cache.get(scope);
    if (infos !== undefined) return infos;
    infos = new ScopeInfos(scope, this.byCallstacks);
    this.cache.set(scope, infos);
    return infos;
  }

  isEmpty(scope: Ast.decl): boolean {
    const infos = this.cache.get(scope);
    return infos ? infos.isEmpty() : true;
  }

  setByCallstacks(scope: Ast.decl, byCallstacks: boolean): void {
    const infos = this.cache.get(scope);
    if (infos) infos.byCallstacks = byCallstacks;
  }

  setGlobalByCallstacks(byCallstacks: boolean): void {
    this.byCallstacks = byCallstacks;
    this.cache.forEach((infos) => infos.byCallstacks = byCallstacks);
  }

  setFolded(scope: Ast.decl, folded: boolean): void {
    const infos = this.cache.get(scope);
    if (infos) infos.folded = folded;
  }

  changeStartingCallstack(scope: Ast.decl, n: number): void {
    const infos = this.cache.get(scope);
    if (infos) infos.startingCallstack = n;
  }

  pin(scope: Ast.decl, loc: Ast.marker): void {
    this.getInfos(scope).pin(loc);
  }

  unpin(scope: Ast.decl, loc: Ast.marker): void {
    this.cache.get(scope)?.pinned.delete(loc);
  }

  removeLocation(scope: Ast.decl, loc: Ast.marker): void {
    const infos = this.cache.get(scope);
    if (infos !== undefined) infos.delete(loc);
  }

  delete(scope: Ast.decl): void {
    this.cache.delete(scope);
  }

  clear(): void {
    this.cache.clear();
  }

  clean(scope: Ast.decl | undefined): void {
    this.cache.forEach((infos) => {
      if (infos.scope !== scope && infos.isEmpty())
        this.cache.delete(infos.scope);
    });
  }

  map<A>(fn: (infos: ScopeInfos) => A): A[] {
    const data: A[] = [];
    this.cache.forEach((e) => data.push(fn(e)));
    return data;
  }

}

/* -------------------------------------------------------------------------- */
/* --- Eva Table Complete Component                                       --- */
/* -------------------------------------------------------------------------- */

/* Table's state. It is global for when the user changes the view. */
export const CallstackState = new GlobalState<callstack>('Summary');
const ScopesManagerState = new GlobalState(new ScopesManager());
const FocusState = new GlobalState<Probe | undefined>(undefined);
const ticValue = new GlobalState<number>(0);

export function clearGlobalState(): void {
  CallstackState.setValue('Summary');
  ScopesManagerState.setValue(new ScopesManager());
  FocusState.setValue(undefined);
}

{
  Server.onSignal(currentProject.signal, clearGlobalState);
}

/* Component */
function EvaTable(): JSX.Element {

  const [showCallstacks, flipCallstacks] =
    Dome.useFlipSettings('ivette.eva.showCallstacks', false);

  /* Component state */
  const marker = States.useSelected();
  const [cs, setCS] = useGlobalState(CallstackState);
  const [fcts] = useGlobalState(ScopesManagerState);
  const [focus, setFocus] = useGlobalState(FocusState);

  /* Used to force the component update. We cannot use the `forceUpdate` hook
   * proposed by Dome as we need to be able to add dependencies on a changing
   * value (here tac) explicitly. We need to force the update as modifications
   * of the Scope Manager internal data does NOT trigger the component
   * update. */
  const [globalTac, ] = useGlobalState(ticValue);
  const [tac, setTic] = React.useState(0);
  React.useEffect(() => setTic(t => t+1), [globalTac]);

  /* Event use to communicate when a location is selected. Used to scroll
   * related column into view if needed */
  const [locEvt] = React.useState(new Dome.Event<Ast.marker>('eva-location'));

  /* Needed server's requests */
  const getProbe = useProbe();
  const getCallsites = useCallsites();
  const getCallstacks = useCallstacks();

  /* Updates the scope manager when the showCallstacks state changes. */
  React.useEffect(() => {
    fcts.setGlobalByCallstacks(showCallstacks);
    setTic(tac => tac + 1);
  }, [fcts, showCallstacks]);

  /* Computing the function corresponding to the selected callstack */
  const csFctPromise = React.useMemo(async () => {
    const selectedCSInfos = await getCallsites(cs);
    if (selectedCSInfos.length === 0) return undefined;
    else return selectedCSInfos[0].callee;
  }, [cs, getCallsites]);
  const { result: csFct } = Dome.usePromise(csFctPromise);

  /* Reset the selected callstack when the corresponding function is removed */
  React.useEffect(() => {
    if (csFct && fcts.isEmpty(csFct) && focus?.scope !== csFct)
      setCS('Summary');
  }, [csFct, setCS, fcts, focus?.scope]);

  /* Updated the focused Probe when the selection changes. Also emit on the
   * `locEvent` event. */
  React.useEffect(() => {
    const scope = States.getMarker(marker).scope;
    fcts.clean(scope);
    const doUpdate = (p: Probe): void => {
      if (!p.evaluable) { setFocus(undefined); return; }
      if (scope && p.code) fcts.newScope(scope);
      setFocus(p);
      if (marker) locEvt.emit(marker);
    };
    if (scope && marker) getProbe([scope, marker]).then(doUpdate);
    else setFocus(undefined);
  }, [marker, fcts, getProbe, setFocus, locEvt]);

  /* Callback used to pin or unpin a location */
  const setLocPin = React.useCallback(
    (scope: Ast.decl, marker: Ast.marker, pin: boolean): void => {
      if (pin) fcts.pin(scope, marker);
      else fcts.unpin(scope, marker);
      setTic(tac + 1);
    }, [fcts, setTic, tac]);

  /* On meta-selection, pin the selected location. */
  React.useEffect(() => {
    const pin = (loc: States.Location): void => {
      const { marker } = loc;
      const scope = States.getMarker(marker).scope;
      if (scope && marker) setLocPin(scope, marker, true);
    };
    States.MetaSelection.on(pin);
    return () => States.MetaSelection.off(pin);
  });

  /* Callback used to remove a probe */
  const remove = React.useCallback((probe: Probe): void => {
    fcts.removeLocation(probe.scope, probe.marker);
    if (probe.marker === focus?.marker) {
      setFocus(undefined);
      fcts.clean(undefined);
    }
    else {
      fcts.clean(focus?.scope);
    }
    setTic(tac + 1);
  }, [fcts, focus, setFocus, tac]);

  /* Builds the sections for each function. As the component is built
   * asynchronously, we have to use the `usePromise` hook, which forces us to
   * memoize the promises building. */
  const functionsPromise = React.useMemo(() => {
    const elts: Promise<JSX.Element>[] = fcts.map((fct: ScopeInfos) => {
      const { byCallstacks, scope, folded } = fct;
      const isSelectedCallstack = (c: callstack): boolean => c === cs;
      const setFolded = (folded: boolean): void => {
        fcts.setFolded(scope, folded);
        setTic(tac + 1);
      };
      const setByCS = (byCS: boolean): void => {
        fcts.setByCallstacks(fct.scope, byCS);
        setTic(tac + 1);
      };
      const changeStartingCallstack = (n: number): void => {
        fcts.changeStartingCallstack(scope, n);
        setTic(tac + 1);
      };
      const close = (): void => {
        fcts.delete(scope);
        if (csFct === scope) setCS('Summary');
        setTic(tac + 1);
      };
      return ScopeSection({
        markers: fct.markers(focus),
        scope,
        close,
        pinProbe: (loc: Ast.marker, pin) => { setLocPin(scope, loc, pin); },
        getProbe,
        selectProbe: setFocus,
        removeProbe: remove,
        folded,
        setFolded,
        getCallsites,
        byCallstacks,
        getCallstacks,
        setByCallstacks: setByCS,
        selectCallstack: (c: callstack) => { setCS(c); setTic(tac + 1); },
        isSelectedCallstack,
        locEvt,
        startingCallstack: fct.startingCallstack,
        changeStartingCallstack,
      });
    });
    return Promise.all(elts);
  }, [
    cs, setCS, fcts, focus, setFocus, tac,
    getCallsites, setLocPin, csFct,
    getCallstacks, getProbe, remove, locEvt
  ]);
  const { result: functions } = Dome.usePromise(functionsPromise);

  /* Builds the alarms component. As for the function sections, it is an
   * asynchronous process. */
  const alarmsProm = React.useMemo(() => AlarmsInfos(focus)(cs), [focus, cs]);
  const { result: alarmsInfos } = Dome.usePromise(alarmsProm);

  /* Builds the stacks component. As for the function sections, it is an
   * asynchronous process. */
  const stackInfosPromise = React.useMemo(async () => {
    const callsites = await getCallsites(cs);
    const p = (c: Values.callsite): boolean =>
      c.stmt !== undefined && c.stmt === marker;
    const isSelected = callsites.find(p) !== undefined;
    const close = (): void => setCS('Summary');
    return StackInfos({ callsites, isSelected, close });
  }, [cs, setCS, getCallsites, marker]);
  const { result: stackInfos } = Dome.usePromise(stackInfosPromise);

  /* Clear the table when Eva values change. */
  const clear = (): void => {
    fcts.clear();
    setTic(tac + 1);
  };
  Server.useSignal(Values.changed, clear);

  /* Builds the component */
  return (
    <>
      <Ivette.TitleBar help="eva-values">
        <IconButton
          icon="ITEMS.LIST"
          title="Show values by callstack by default"
          selected={showCallstacks}
          onClick={flipCallstacks}
        />
        <Inset />
        <EvaStatus />
      </Ivette.TitleBar>
      <EvaReady>
        <div className="eva-functions-section">
          {React.Children.toArray(functions)}
        </div>
        <Vfill />
        {alarmsInfos}
        {stackInfos}
      </EvaReady>
    </>
  );

}

/* -------------------------------------------------------------------------- */
/* --- Evaluation Modal                                                   --- */
/* -------------------------------------------------------------------------- */

interface ModalTextFieldProps {
  attr: Ast.markerAttributesData;
}

function ModalEvaluate(props: ModalTextFieldProps): React.JSX.Element {
  const { attr } = props;
  const state = useState('');
  const value = state.value;
  const [error, setError] = React.useState<string | undefined>();
  const [fcts] = useGlobalState(ScopesManagerState);
  const [tac, setTic] = useGlobalState(ticValue);

  const onValidate = React.useCallback((pattern: string): void => {
    const data = { stmt: attr.marker, term: pattern };
    const addProbe = (target: Ast.marker): void => {
      closeModal();
      const scope = States.getMarker(attr.marker).scope;
      if (scope) {
        fcts.pin(scope, target);
        setTic(tac + 1);
        Display.showMessage(`New expression ${pattern} in Values table`);
        Display.alertComponent('fc.eva.values');
      }
    };
    Server.send(Ast.parseExpr, data).then(addProbe).catch(setError);
  }, [attr, fcts, tac, setTic]);

  return (
    <Modal className='modal-evaluate' label={'Eva: evaluate expression'}>
      <div>
        <Label>Compute the possible values of: </Label>
        <Hbox>
          <TextField
            label=''
            latency={0}
            autoFocus={true}
            state={state as FieldState<string | undefined>}
            onKeyDown={(e) => { if (e.key === "Enter") onValidate(value); }}
          />
          <Button
            label='Evaluate'
            onClick={() => onValidate(value)}
          />
        </Hbox>
        { error &&
          <Hbox>
            <Icon id='WARNING' kind='warning' />
            <span>{error}</span>
          </Hbox> }
      </div>
    </Modal>);
}

async function showEvaluate(attr: Ast.markerAttributesData): Promise<void> {
  showModal(<ModalEvaluate attr={attr}/>);
}

/* Builds the 'evaluate' entries in the contextual menu about a given marker. */
export function buildMenu(
  menu: Dome.PopupMenuItem[],
  attr: Ast.markerAttributesData,
): void {
  if(evaComputationValue === 'computed' && attr.kind === 'STMT') {
    menu.push({
      label: 'Evaluate…',
      enabled: true,
      onClick: () => showEvaluate(attr)
    });
  }
}

ASTview.registerMarkerMenuExtender(buildMenu);

/* Registers the component in Ivette */
Ivette.registerComponent({
  id: 'fc.eva.values',
  label: 'Eva Values',
  title: 'Values inferred by the Eva analysis',
  children: <EvaTable />,
});

/* -------------------------------------------------------------------------- */
