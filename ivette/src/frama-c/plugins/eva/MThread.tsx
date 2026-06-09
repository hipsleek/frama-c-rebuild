/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import * as Ivette from 'ivette';

import { useFlipSettings, useStringSettings } from 'dome';
import { Button, ButtonGroup, ToolBar } from 'dome/frame/toolbars';
import { useModel } from 'dome/table/models';
import { Label } from 'dome/controls/labels';
import { Icon } from 'dome/controls/icons';
import { IconButton } from 'dome/controls/buttons';
import { State } from 'dome/data/states';
import * as Forms from 'dome/layout/forms';
import { LED, LEDstatus } from 'dome/controls/displays';
import { useColorTheme, useStyle } from 'dome/themes';
import { HelpButton } from 'dome/help';

import * as States from 'frama-c/states';
import * as Locations from 'frama-c/kernel/Locations';
import { marker } from 'frama-c/kernel/api/ast';

import {
  mtSharedVarsSummary,
  mtSharedVarsSummaryData,
  mtThreadsSummary,
  mtThreadsSummaryData,
  protectionKind
} from './api/mthread';

import { EvaReady, EvaStatus } from './components/AnalysisStatus';
import { AnimatePresence, motion } from 'framer-motion';
import { classes } from 'dome/misc/utils';

interface MtContext {
  selectedThread?: [string, (v: string) => void];
  selectedMutex?: [string, (v: string) => void];
  selectedMessage?: [string, (v: string) => void];
  selectedVar?: [string, (v: string) => void];
}

const MTCONTEXT = React.createContext<MtContext>({});
const animateOption = {
  layout: true,
  initial: { opacity: 0, scale: .5 },
  animate: { opacity: 1, scale: 1 },
  exit: { opacity: 0, scale: .5 },
  transition: { duration: .5 },
};

// ----------------------------------------------------------------------------
// --- MThread buttons
// ----------------------------------------------------------------------------

interface MtButtonProps {
  label: string;
  id: number | string;
  selectedState?: [string, (v: string) => void];
  onClick?: () => void;
}

function MtButton(props: MtButtonProps): React.JSX.Element {
  const { label, id, selectedState } = props;
  const strId = typeof id === 'number' ? id.toString() : id;

  /** If a zone corresponding to a table or table element is selected,
    all the elements in the table are highlighted. */
  const selected = selectedState &&
    selectedState[0].replace(/\[.*\]$/, "") === strId.replace(/\[.*\]$/, "");
  const onClick = (): void => {
    if(selectedState)
      if(selected) selectedState[1]("");
      else selectedState[1](strId);
    if(props.onClick) props.onClick();
  };

  return <Button
    key={strId}
    label={label}
    title={label}
    selected={selected}
    onClick={onClick}
  />;
}

function getMtButtons(
  data: [number, string][] | string[],
  selectedState?: [string, (v: string) => void],
  onClick?: () => void
): React.JSX.Element[] {
  return data.map(val => {
    const isArray = Array.isArray(val);
    const id = isArray ? val[0] : val;
    const label = isArray ? val[1] : val;
    return <MtButton key={id} id={id} label={label}
      selectedState={selectedState}
      onClick={onClick}
      />;
    }
  );
}

// ----------------------------------------------------------------------------
// --- MThread Element
// ----------------------------------------------------------------------------

interface ElementProps {
  depth?: 'primary' | 'secondary',
  mode?: 'row' | 'column';
  className?: string;
  title?: React.ReactNode;
  separate?: boolean;
  animate?: boolean;
  children?: React.ReactNode;
}

function Element(props: ElementProps): React.JSX.Element {
  const { title, mode, className, depth,
    separate, animate = false, children } = props;
  const style = useStyle();
  const isDark = useColorTheme()[0] === 'dark';

  const bgColor = !depth ? undefined : depth === 'primary' ?
    style.getPropertyValue('--background-intense'):
    isDark ?
      style.getPropertyValue('--background-block-form'):
      style.getPropertyValue('--background-alterning-odd');

  const classesElt = classes('mthread-element', className);
  const classesContent = classes(
    'mthread-element-content',
    `mthread-element-content-${mode}`,
    Boolean(title && separate) && 'mthread-element-content-separator'
  );
  const options = {
    className: classesElt,
    style: { backgroundColor: bgColor }
  };

  const hasChildren = Array.isArray(children) ?
    children.some(e => e !== null) : Boolean(children);

  const content = <>
    { title &&
        typeof(title) === "string" ?
          <Label title={title}>{title}</Label>
          : <div className='mthread-element-title'>{ title }</div>
    }
    { hasChildren && <div className={classesContent}>{ children }</div> }
  </>;

  return <>
    { animate
      ? <motion.div {...animateOption} {...options}>{ content }</motion.div>
      : <div {...options}>{ content }</div>
    }
  </>;
}

// ----------------------------------------------------------------------------
// --- Thread Content
// ----------------------------------------------------------------------------

function isErrorInMutex(thread: mtThreadsSummaryData): boolean {
  const taken = thread.locksTaken;
  const released = thread.locksReleased;
  return !taken.every(
    (lock) => released.find(e => e[0] === lock[0])
  ) || released.length !== taken.length;
}

function getFilteredThread(
  threads: mtThreadsSummaryData[],
  filterErrorMutexState: [boolean, () => void],
): mtThreadsSummaryData[] {
    return !filterErrorMutexState[0]
      ? threads
      : threads.filter((mt) => isErrorInMutex(mt));
}

interface ContentProps {
  data:  mtThreadsSummaryData;
  showEmpty: boolean;
}

function ErrorContent(props: ContentProps): React.JSX.Element | null {
  const errors: string[] = [];
  if(isErrorInMutex(props.data))
    errors.push("Mutex: the mutexes taken and released are not the same ");
  const label = errors.length > 0 ? 'Error'
    : `No errors in ${props.data.thread[1]}`;
  const icon = errors.length > 0 ? "WARNING" : undefined;
  const iconKind = errors.length > 0 ? "negative" : undefined;
  const title = <>
    <Label label={label} title={label}/>
    { icon && <Icon id={icon} kind={iconKind} /> }
  </>;
  const content = errors.map((error, i) =>
    <Element  key={i} title={error} mode='row' depth='secondary' />);

  if(!props.showEmpty && errors.length === 0) return null;
  return (
    <Element title={title} depth='primary'>
      { content.length > 0 ? content : null }
    </Element>
  );
}

function MutexContent(props: ContentProps): React.JSX.Element | null {
  const { data: { locksTaken, locksReleased }, showEmpty } = props;
  const { selectedMutex } = React.useContext(MTCONTEXT);
  const isTaken = locksTaken.length > 0;
  const isReleased = locksReleased.length > 0;
  const nonEmpty = Boolean(isTaken || isReleased);
  const label = nonEmpty ? 'Mutex': 'Mutex: no mutex taken or released';

  if(!showEmpty && !nonEmpty) return null;
  const takenContent = isTaken ?
    <Element title='Taken' mode='row' depth='secondary' separate={true}>
      {getMtButtons(locksTaken, selectedMutex)}
    </Element> : null;
  const releasedContent = isReleased ?
    <Element title='Released' mode='row' depth='secondary' separate={true}>
      {getMtButtons(locksReleased, selectedMutex)}
    </Element> : null;

  return (
    <Element title={label} depth='primary' className='mthread-element-mutex'>
      { takenContent }
      { releasedContent }
    </Element>
  );
}
function MessageContent(props: ContentProps): React.JSX.Element | null {
  const { data, showEmpty } = props;
  const { selectedMessage } = React.useContext(MTCONTEXT);
  const { mqueuesCreated, mqueuesReceivers, mqueuesSenders } = data;
  const isCreate = mqueuesCreated.length > 0;
  const isReceived = mqueuesReceivers.length > 0;
  const isSend = mqueuesSenders.length > 0;
  const nonEmpty = Boolean(isCreate || isReceived || isSend);
  const label = nonEmpty ? 'Message': 'Message: no message';

  if(!showEmpty && !nonEmpty) return null;

  const creContent = isCreate ?
    <Element title='Create' mode='row' depth='secondary' separate={true}>
      {getMtButtons(mqueuesCreated, selectedMessage)}
    </Element> : null;
  const recContent = isReceived ?
    <Element title='Receive' mode='row' depth='secondary' separate={true}>
      {getMtButtons(mqueuesReceivers, selectedMessage)}
    </Element>  : null;
  const sendContent = isSend ?
    <Element title='Send' mode='row' depth='secondary' separate={true}>
      {getMtButtons(mqueuesSenders, selectedMessage)}
    </Element> : null;

  return (
    <Element title={label} depth='primary' className='mthread-element-message'>
      { creContent }
      { recContent }
      { sendContent }
    </Element>
  );
}

function VariableContent(props: ContentProps): React.JSX.Element | null {
  const { data: { sharedVarsRead, sharedVarsWritten }, showEmpty } = props;
  const { selectedVar } = React.useContext(MTCONTEXT);
  const isVarRead = sharedVarsRead.length > 0;
  const isVarWrite = sharedVarsWritten.length > 0;
  const nonEmpty = Boolean(isVarRead || isVarWrite);
  const label = nonEmpty ?
    'Variable':
    'Variable: no variable read or written';

  if(!showEmpty && !nonEmpty) return null;
  const readContent = isVarRead ?
    <Element title='Read' mode='row' depth='secondary' separate={true}>
     {getMtButtons(sharedVarsRead, selectedVar)}
   </Element> : null;
  const writeContent = isVarWrite ?
    <Element title='Write' mode='row' depth='secondary' separate={true}>
      {getMtButtons(sharedVarsWritten, selectedVar)}
    </Element> : null;

  return (
    <Element title={label} depth='primary' mode='row'
      className='mthread-element-variable'>
        { readContent }
        { writeContent }
    </Element>
  );
}

interface ThreadsProps {
  threads: mtThreadsSummaryData[],
  showEmpty: boolean,
  errors: boolean,
  mutex: boolean,
  message: boolean,
  variable: boolean,
}

function Threads(props: ThreadsProps): React.ReactNode {
  const { threads, showEmpty, errors, mutex, message, variable } = props;
  return threads.map((t) => {
      const data = { data: t, showEmpty: showEmpty };
      const [ id, name ] = t.thread;
      return (
        <Element key={id} title={name} animate={true}
          className='mthread-element-container'>
          {errors && <ErrorContent {...data} />}
          {mutex && <MutexContent {...data} />}
          {message && <MessageContent {...data} />}
          {variable && <VariableContent {...data} />}
        </Element>
      );
  });
}

// ----------------------------------------------------------------------------
// --- Variables
// ----------------------------------------------------------------------------

type ProtectionKind =  keyof typeof protectionKind ;
interface ProtectionKindInfos {
  label: string,
  LEDStatus: LEDstatus
}

const protectionKindInfos: {[key in protectionKind]: ProtectionKindInfos} = {
    protected: {
      label: 'Protected',
      LEDStatus: 'positive'
    },
    // eslint-disable-next-line camelcase
    maybe_protected: {
      label: 'Maybe protected',
      LEDStatus: 'warning'
    },
    unprotected: {
      label: 'Unprotected',
      LEDStatus: 'negative'
    }
};

type VarsByBase = { [base: string] : VarsByBaseContent }
type VarsByZone = { [zone: string] : VarsByZoneContent }
type VarsByZoneContent = Omit<VarsByBaseContent, 'zoneNames'>
interface VarsByBaseContent {
  read: mtSharedVarsSummaryData[];
  write: mtSharedVarsSummaryData[];
  protectionKind: ProtectionKind;
  zoneNames: Set<string>;
  zones?: VarsByZone;
}

interface VariableProps {
  vars: VarsByBase;
  varPinState: State<string | undefined>;
  showProtected: boolean;
  showMProtected: boolean;
  showUnProtected: boolean;
  showVar: boolean;
  showArray: boolean;
  searchVarName: string | undefined;
}

interface VarContentProps {
  base: string;
  data: mtSharedVarsSummaryData[];
  accessKind: "read" | "write";
}

interface ItemVarByProtectionKind extends VarContentProps {
  kind: ProtectionKind;
}

function ItemVarByProtectionKind(
  props: ItemVarByProtectionKind
): React.JSX.Element | null {
  const { base, kind, accessKind } = props;
  const data = props.data.filter(e => e.protectionKind === kind);
  const allMarkers = data.map(e => e.markers).flat();
  const kindLabel = protectionKindInfos[kind].label;

  const onClick = (m: marker[], label: string): () => void => {
    return () => Locations.setSelection({
      plugin: 'MThread',
      label: label,
      markers: m
    });
  };

  const buttons: React.JSX.Element[] = [];
  data.forEach(val => {
    if(!val.protectionMutexes) return;
    else val.protectionMutexes.forEach(v => {
      const id = v[0];
      const zoneLabel = base !== val.zones ? `(${val.zones})` : "";
      const label = `${v[1]} ${zoneLabel}`;
      const locationLabel =
        `${base} : ${accessKind} : ${kindLabel.toLowerCase()} : ${label}`;
      buttons.push(<MtButton
        key={`${id}-${zoneLabel}`}
        id={id}
        label={label}
        onClick={onClick(val.markers || [], locationLabel)}
      />
      );
    });
  });

  if(allMarkers.length === 0) return null;

  const actions = <IconButton
    icon='MULTICHECK'
    title={`Select all "${kindLabel}" access`}
    onClick={onClick(allMarkers,
      `${base} : ${accessKind} : ${kindLabel.toLowerCase()}`)}
  />;

  return (
    <Element title={<>{ kindLabel }{ actions }</>}
      mode='row'
      depth='secondary'
      separate={true}
    >{ buttons.length > 0 && buttons }</Element>
  );
}

function VarContent(props: VarContentProps): React.JSX.Element | null {
  const { accessKind } = props;
  const emptyContent = props.data.length === 0;
  const ledStatus = protectionKindInfos[getProtectionByAccesskind(props.data)]
    .LEDStatus;
  const label = React.useMemo(() => {
      if(accessKind === "read") return !emptyContent ?
      'Read' : 'The variable is never read';
      else return !emptyContent ?
      'Write' : 'The variable is never written';
  }, [accessKind, emptyContent]);
  const title = <>
      <Label label={label} title={label} />
      { ledStatus && <LED status={ledStatus} /> }
    </>;

  return (
    <Element title={title} depth='primary' >
      <ItemVarByProtectionKind kind={'protected'} {...props} />
      <ItemVarByProtectionKind kind={'maybe_protected'} {...props} />
      <ItemVarByProtectionKind kind={'unprotected'} {...props} />
    </Element>
  );
}

function getMarker(
  vars: VarsByBaseContent, access: 'all' | 'read' | 'write'
): marker[] {
  let markers: marker[] = [];

  if(access === 'all' || access === 'read')
    markers = markers.concat(vars['read'].flatMap(v => v.markers));

  if(access === 'all' || access === 'write')
    markers = markers.concat(vars['write'].flatMap(v => v.markers));

  return [...new Set(markers)];
}

// --- Check protection
interface ProtectionData {
  protected: Set<number>; // List of protected variable IDs
  maybe_protected: Set<number>; // List of maybe protected variable IDs
  unprotected: number; // Number of unprotected variables
}

function checkBaseProtection(v: VarsByBaseContent): ProtectionKind {
  if(!v.zones) return checkZoneProtection(v);
  return Object.entries(v.zones).every(e =>
    e[1].protectionKind === 'protected') ? 'protected' : 'maybe_protected';
}

function fillProtectionData(
  line: mtSharedVarsSummaryData, p: ProtectionData
): void {
  switch(line.protectionKind) {
    case 'protected': line.protectionMutexes.forEach(([id,]) =>
      p.protected.add(id)); break;
    case 'maybe_protected': line.protectionMutexes.forEach(([id,]) =>
      p.maybe_protected.add(id)); break;
    case 'unprotected':
      p.unprotected++; break;
  }
}

function getProtectionData(data: mtSharedVarsSummaryData[]): ProtectionData {
  const protection: ProtectionData = {
    // eslint-disable-next-line camelcase
    protected: new Set(), maybe_protected: new Set(), unprotected: 0
  };
  data.forEach(e => fillProtectionData(e, protection));
  return protection;
}

function getProtectionByAccesskind(
  data: mtSharedVarsSummaryData[]
): ProtectionKind {
  const protection = getProtectionData(data);
  function isProtected(): boolean { return protection.protected.size > 0; }
  function isMaybeProtected(): boolean {
    return protection.maybe_protected.size > 0;
  }
  return isProtected() ? 'protected'
    : isMaybeProtected() ? 'maybe_protected'
    : 'unprotected';
}

function checkZoneProtection(v: VarsByZoneContent): ProtectionKind {
  const pRead = getProtectionData(v.read);
  const pWrite = getProtectionData(v.write);

  function isProtected(): boolean {
    return pRead.protected.size > 0 && pWrite.protected.size > 0
        && [...pRead.protected].some(id => pWrite.protected.has(id));
    }

  function isMaybeProtected(): boolean {
    return pRead.unprotected === 0 && pWrite.unprotected === 0;
  }

  return isProtected() ? 'protected'
    : isMaybeProtected() ? 'maybe_protected' : 'unprotected';
}

// --- get structured data
function getByZone(v: VarsByBaseContent): VarsByZone {
  const vars = v.read.concat(v.write);
  const byZones: VarsByZone = {};

  vars.forEach(line => {
    if(!byZones[line.zones]) byZones[line.zones] = {
      read: [], write: [], protectionKind: 'unprotected'
    };
    byZones[line.zones][line.accessKind].push(line);
  });

  Object.entries(byZones).forEach(e => {
    byZones[e[0]].protectionKind = checkZoneProtection(e[1]);
  });

  return byZones;
}

function getByBase(v: mtSharedVarsSummaryData[]): VarsByBase {
  const byBases: VarsByBase = {};
  v.forEach(line => {
    if(!byBases[line.bases]) byBases[line.bases] = {
      read: [], write: [], protectionKind: 'unprotected',
      zoneNames: new Set<string>()
    };
    byBases[line.bases][line.accessKind].push(line);
    byBases[line.bases].zoneNames.add(line.zones);
  });

  Object.entries(byBases).forEach(e => {
    if(e[1].zoneNames.size > 1) byBases[e[0]].zones = getByZone(e[1]);
    byBases[e[0]].protectionKind = checkBaseProtection(e[1]);
  });

  return byBases;
}

// --- hook for variable
function useVariables(): VarsByBase {
  const modelVars = States.useSyncArrayModel(mtSharedVarsSummary);
  const syncModelVars = useModel(modelVars);

  const newVars = React.useMemo(() => {
    syncModelVars; // fake used to avoid warning
    return getByBase(modelVars.getArray());
  }, [modelVars, syncModelVars]);
  return newVars;
}

function Variable(props: VariableProps): React.ReactNode {
  const { vars, varPinState, showProtected, showMProtected,
    showUnProtected, showArray, showVar, searchVarName } = props;
  const [ varPin, setvarPin ] = varPinState;

  /** The content change if an array is pined */
  const filterVars: VarsByBase = React.useMemo(() => {
    if(!varPin) return vars;
    const newVars = {};
    const content = vars[varPin];
    if(!content.zones) return vars;
    const base = Object.fromEntries([[varPin, content]]);
    Object.assign(newVars, base, content.zones);
    return newVars;
  }, [vars, varPin]);

  const isVisible = React.useCallback(
    /**
     * Checks whether the variable is visible according to the
     * filtersButton (showVar, showArray, showProtected, etc.)
     * and based on the text filter ‘searchVarName’.
     * If one member of the disjunction is true, the variable must be invisible
     */
    (base: string, content: VarsByBaseContent ) => {
      return !(
        searchVarName && !base.includes(searchVarName)
        || !showArray && content.zones // zones exist only in array
        || !showVar && !content.zones // zones exist only in array
        || !showProtected && content.protectionKind === 'protected'
        || !showMProtected && content.protectionKind === 'maybe_protected'
        || !showUnProtected && content.protectionKind === 'unprotected'
      );
  }, [ searchVarName, showArray, showVar,
    showProtected, showMProtected, showUnProtected ]);

  function getSelectionButton(base: string, data: VarsByBaseContent)
  : React.JSX.Element {
    return <IconButton icon='MULTICHECK' title='Select all access'
      onClick={() => Locations.setSelection({
          plugin: 'MThread',
          label: `Accesses for ${base}`,
          markers: getMarker(data, 'all')
        })
      }
    />;
  }

  function getPinButton(base: string): React.JSX.Element {
    return <IconButton icon='TABLE' title={`Filter on base ${base}`}
        kind={varPin === base ? 'selected' : 'default'}
        onClick={() => setvarPin(varPin === base ? undefined : base)
      }
    />;
  }

  return Object.entries(filterVars).map((v) => {
    const [ base, data ] = v;
    const varStatus = protectionKindInfos[data.protectionKind].LEDStatus;
    const isArray = Boolean(data.zones);
    const label = isArray ? `${base}[ ]` : base;
    const title = <>
      <LED status={varStatus} />
      <Label title={label}>{label}</Label>
      <div className='action'>
        { getSelectionButton(base, data) }
        { isArray && getPinButton(base) }
      </div>
    </>;
    return isVisible(base, data) ?
      <Element key={base} title={title} animate={true}>
        <VarContent accessKind='read' base={base} data={data.read} />
        <VarContent accessKind='write' base={base} data={data.write} />
      </Element>
      : null;
  });
}

// ----------------------------------------------------------------------------
// --- MThread Toolbar
// ----------------------------------------------------------------------------

interface ThreadToolbarProps {
  showEmptyState: [boolean, () => void];
  showErrorsState: [boolean, () => void];
  showMutexState: [boolean, () => void];
  showMessageState: [boolean, () => void];
  showVariableState: [boolean, () => void];
  showThreadWithErrorOnlyState: [boolean, () => void];
}

function ThreadToolbar(props: ThreadToolbarProps)
: React.JSX.Element {
  const [ showEmpty, setshowEmpty ] = props.showEmptyState;
  const [ errors, setErrors ] = props.showErrorsState;
  const [ mutex, setMutex ] = props.showMutexState;
  const [ message, setMessage ] = props.showMessageState;
  const [ variable, setVariable ] = props.showVariableState;
  const [ showThreadWithErrorOnly, setShowThreadWithErrorOnly ] =
    props.showThreadWithErrorOnlyState;

  return <>
    <div>
      <Button label={'Show empty'} title='show Errors' selected={showEmpty}
        onClick={() => setshowEmpty()} />
      <ButtonGroup>
        <Button label='Errors' title='show Errors' selected={errors}
          onClick={() => setErrors()} />
        <Button label='Mutex' title='show mutex' selected={mutex}
          onClick={() => setMutex()}  />
        <Button label='Message' title='show message' selected={message}
          onClick={() => setMessage()} />
        <Button label='Variable' title='show variable' selected={variable}
          onClick={() => setVariable()} />
      </ButtonGroup>
    </div>
    <Button label='Thread with error only' title='Show only thread with error'
      selected={showThreadWithErrorOnly}
      onClick={() => setShowThreadWithErrorOnly()} />
  </>;
}

interface  VarToolbarProps {
  varNameState: Forms.FieldState<string | undefined>,
  showProtectedState: [boolean, () => void];
  showMProtectedState: [boolean, () => void];
  showUnProtectedState: [boolean, () => void];
  showVarState: [boolean, () => void];
  showArrayState: [boolean, () => void];
}

function VarToolbar(props: VarToolbarProps)
: React.JSX.Element {
  const { varNameState, showArrayState, showVarState,
    showProtectedState, showMProtectedState, showUnProtectedState } = props;
  const [ showArray, flipShowArray ] = showArrayState;
  const [ showVar, flipShowVar ] = showVarState;
  const [ showProtected, flipShowProtected ] = showProtectedState;
  const [ showMProtected, flipShowMProtected ] = showMProtectedState;
  const [ showUnProtected, flipShowUnProtected ] = showUnProtectedState;

  return <>
    <div className='mthread-variable-filter'>
      <Forms.TextField
        label=''
        placeholder='Search by name'
        state={varNameState}
        actions={<IconButton
          icon='TRASH'
          onClick={() => varNameState.onChanged(undefined, undefined, false)}
        />}
      />
      <ButtonGroup>
        <Button icon='VARIABLE' title='show variable' selected={showVar}
          onClick={() => flipShowVar()} />
        <Button icon='TABLE' title='show array' selected={showArray}
          onClick={() => flipShowArray()} />
      </ButtonGroup>
      <ButtonGroup>
        <Button label='Protected' title='show protected variable'
          selected={showProtected}
          onClick={() => flipShowProtected()} />
        <Button label='Maybe protected' title='show maybe protected variable'
          selected={showMProtected}
          onClick={() => flipShowMProtected()} />
        <Button label='Unprotected' title='show unprotected variable'
          selected={showUnProtected}
          onClick={() => flipShowUnProtected()} />
      </ButtonGroup>
    </div>
  </>;
}

interface  MThreadToolbarProps
extends ThreadToolbarProps, VarToolbarProps {
  displayModeState: State<string>;
}

function MThreadToolbar(props: MThreadToolbarProps)
: React.JSX.Element {
  const [ displayMode, setDisplayMode] = props.displayModeState;

  return (
    <ToolBar className={'eva-mthread-toolbar'}>
      <ButtonGroup>
        <Button label='Thread' title='Thread mode'
          selected={displayMode === 'thread'}
          onClick={() => setDisplayMode('thread')} />
        <Button label='Variable' title='Variable mode'
          selected={displayMode === 'variable'}
          onClick={() => setDisplayMode('variable')} />
      </ButtonGroup>
      { displayMode === 'thread' && <ThreadToolbar {...props} /> }
      { displayMode === 'variable' && <VarToolbar {...props} /> }
    </ToolBar>
  );
}

// ----------------------------------------------------------------------------
// --- MThread component
// ----------------------------------------------------------------------------

function MThreadComponent(): JSX.Element {
  const base = 'ivette.mthread.show';
  const displayModeState = useStringSettings(`${base}.mode`, 'thread');
  const [ displayMode, ] = displayModeState;

  const context = React.useContext(MTCONTEXT);
  context.selectedThread = React.useState("");
  context.selectedMutex = React.useState("");
  context.selectedMessage = React.useState("");
  context.selectedVar = React.useState("");

  // --- Threads
  const showEmptyState = useFlipSettings(`${base}.empty`, false);
  const showErrorsState = useFlipSettings(`${base}.errors`, false);
  const showMutexState = useFlipSettings(`${base}.mutex`, false);
  const showMessageState = useFlipSettings(`${base}.message`, false);
  const showVariableState = useFlipSettings(`${base}.variable`, false);
  const showThreadWithErrorOnlyState =
    useFlipSettings(`${base}.thread.with.error.only`, false);
  const modelThread = States.useSyncArrayModel(mtThreadsSummary);
  const syncModelThread = useModel(modelThread);
  const threads = React.useMemo(() => {
    syncModelThread; // fake used to avoid warning
    return modelThread.getArray();
  }, [syncModelThread, modelThread]);

  const threadsFiltering = React.useMemo(() =>
    getFilteredThread(threads, showThreadWithErrorOnlyState)
  , [threads, showThreadWithErrorOnlyState]);

  // --- Variables
  const vars = useVariables();
  const baseFilterState = React.useState<string | undefined>();
  const varNameState = Forms.useState<string | undefined>(undefined);
  const showProtected = useFlipSettings(`${base}.protected`, true);
  const showMProtected = useFlipSettings(`${base}.mprotected`, true);
  const showUnProtected = useFlipSettings(`${base}.unprotected`, true);
  const showVar = useFlipSettings(`${base}.variable`, true);
  const showArray = useFlipSettings(`${base}.array`, true);

  return (
    <div className='eva-mthread'>
      <Ivette.TitleBar>
        <EvaStatus />
        <HelpButton id={'eva-mthread'} />
      </Ivette.TitleBar>
      <MThreadToolbar
        displayModeState={displayModeState}
        showEmptyState={showEmptyState}
        showErrorsState={showErrorsState}
        showMutexState={showMutexState}
        showMessageState={showMessageState}
        showVariableState={showVariableState}
        showThreadWithErrorOnlyState={showThreadWithErrorOnlyState}
        varNameState={varNameState}
        showProtectedState={showProtected}
        showMProtectedState={showMProtected}
        showUnProtectedState={showUnProtected}
        showVarState={showVar}
        showArrayState={showArray}
      />
      <EvaReady>
        <motion.div className={'eva-mthread-elements'}>
          <AnimatePresence>
            <MTCONTEXT.Provider value={context}>
              { displayMode === 'thread' && <Threads
                  threads={threadsFiltering}
                  showEmpty={showEmptyState[0]}
                  errors={showErrorsState[0]}
                  mutex={showMutexState[0]}
                  message={showMessageState[0]}
                  variable={showVariableState[0]}
                />
              }
              { displayMode === 'variable' && <Variable
                  vars={vars}
                  varPinState={baseFilterState}
                  showProtected = {showProtected[0]}
                  showMProtected = {showMProtected[0]}
                  showUnProtected = {showUnProtected[0]}
                  showVar = {showVar[0]}
                  showArray = {showArray[0]}
                  searchVarName={varNameState.value}
                />
              }
            </MTCONTEXT.Provider>
          </AnimatePresence>
        </motion.div>
      </EvaReady>
    </div>
  );
}

Ivette.registerComponent({
  id: 'fc.eva.mthread',
  label: 'Eva MThread',
  title: 'Eva MThread analysis',
  children: <MThreadComponent />,
});

// ----------------------------------------------------------------------------
