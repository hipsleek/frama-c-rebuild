/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React, { Fragment } from 'react';

import { classes } from 'dome/misc/utils';
import { Icon } from 'dome/controls/icons';
import { IconButton, IconButtonKind } from 'dome/controls/buttons';
import { Spinner, SelectMenu } from 'dome/controls/buttons';
import { Label, Item, Descr } from 'dome/controls/labels';
import { Hbox, Vbox, Filler } from 'dome/layout/boxes';
import { Separator } from 'dome/frame/toolbars';
import * as Dnd from 'dome/dnd';
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as WP from 'frama-c/plugins/wp/api';
import * as TIP from 'frama-c/plugins/wp/api/tip';
import * as TAC from 'frama-c/plugins/wp/api/tac';

type Goal = WP.goal | undefined;
type Node = TIP.node | undefined;
type Prover = WP.prover | undefined;
type Tactic = TIP.tactic | undefined;
type Mode = WP.InteractiveMode;

/* -------------------------------------------------------------------------- */
/* --- Use Actions                                                        --- */
/* -------------------------------------------------------------------------- */

type ProverRequest =
  Server.SetRequest<{ node: TIP.node, provers?: WP.prover[] }, null>;

function sendProver( rq: ProverRequest, node: Node, prover: Prover ): void
{
  if (node && prover) {
    Server.send(rq, { node, provers: [prover] });
  }
}

function sendProverTime( node: Node, prover: Prover, timeout: number): void
{
  if (node && prover && timeout > 0) {
    Server.send(TIP.runProvers, { node, timeout, provers: [prover] });
  }
}

function sendProverInteractive(node: Node, prover: Prover, mode: Mode): void {
  if (node && prover) {
    Server.send(TIP.runProvers, { node, mode, provers: [prover] });
  }
}

function applyTactic(tactic: Tactic): void {
  if (tactic) {
    Server.send(TAC.applyTacticAndProve, tactic);
  }
}

/* -------------------------------------------------------------------------- */
/* --- Prover Feedback                                                    --- */
/* -------------------------------------------------------------------------- */

interface ProverActionProps {
  icon: string;
  kind: IconButtonKind;
  play?: boolean;
  clear?: boolean;
  forward?: number;
  running?: boolean;
}

function getProverActions(result : WP.result) : ProverActionProps {
  switch(result.verdict) {
    case '':
    case 'none':
      return { icon: 'CIRC.INFO', kind: 'default', play: true, clear: false };
    case 'computing':
      return { icon: 'EXECUTE', kind: 'default', running: true, clear: false };
    case 'valid':
      return { icon: 'CIRC.CHECK', kind: 'positive' };
    case 'unknown':
    case 'stepout':
      return { icon: 'CIRC.CLOSE', kind: 'warning' };
    case 'timeout':
      {
        const forward = result.proverTime * 2;
        return { icon: 'CIRC.QUESTION', kind: 'warning', forward };
      }
    case 'failed':
    default:
      return { icon: 'WARNING', kind: 'negative' };
  }
}

interface ProverItemProps {
  node: Node;
  prover: WP.prover;
  result: WP.result;
  inactive?: boolean;
  selected: Prover;
  setSelected: (prv: Prover) => void;
}

function ProverItem(props : ProverItemProps): JSX.Element
{
  const { node, prover, inactive=false, result, selected, setSelected } = props;
  const { name } = States.useSyncArrayElt(WP.ProverInfos, prover) ?? {};
  const { descr='No Result' } = result;
  const { icon: status, kind, running=false, play=false } =
    getProverActions(result);
  const isSelected = prover === selected;
  const className = classes(
    'dome-color-frame wp-tactical-item',
    isSelected && 'selected',
  );
  const enabled = !inactive && (play || running);
  const icon =
    inactive ? 'LOCK' : running ? 'MEDIA.STOP' : 'MEDIA.PLAY';
  const title =
    inactive ? undefined :
    running ? 'Interrupt Prover' : 'Run Prover';
  const action = running ? TIP.killProvers : TIP.runProvers;
  return (
    <Hbox
      className={className}
      onClick={() => setSelected(prover)}
    >
      <Item
        icon={status}
        kind={kind}
        className='wp-tactical-cell'
        label={name}
        title={inactive ? `${descr} (Inactive)` : descr}
      />
      <IconButton
        icon={icon}
        title={title}
        kind={running ? 'warning' : 'positive'}
        enabled={enabled}
        onClick={() => sendProver(action, node, prover)}
      />
    </Hbox>
  );
}

export interface ProverSelection {
  node: Node;
  selected: Prover;
  setSelected: (prv: Prover) => void;
}

export function Provers(props: ProverSelection): JSX.Element {
  const { node, selected, setSelected } = props;
  const { results=[] } = States.useRequestStable(TIP.getNodeInfos, node);
  const provers = States.useSyncValue(WP.provers) ?? [];
  const proversInfo = States.useSyncArrayGetter(WP.ProverInfos);
  const children = [...provers]
    .filter((p) => proversInfo(p)?.active)
    .map((prover) => {
      const res = results.find(([p]) => p === prover);
      const result = res ? res[1] : WP.resultDefault;
      return (
        <Dnd.Item id={prover} key={prover}>
          <ProverItem
            node={node}
            prover={prover}
            result={result}
            selected={selected}
            setSelected={setSelected} />
        </Dnd.Item>
      );
    });
  const isInactive = (p: WP.prover): boolean => (
    p !== 'qed' && p !== 'cfg' && p !== 'script' && !proversInfo(p)?.active
  );
  const inactive =
    results
      .filter(([p]) => isInactive(p))
      .map(([p, r]) => {
        return (
          <ProverItem
            key={p}
            node={node}
            prover={p}
            result={r}
            inactive
            selected={selected}
            setSelected={setSelected} />
        );
      });
  return (
    <>
      <Vbox>
        <Dnd.List items={provers}>
          {node ? children : null}
        </Dnd.List>
      </Vbox>
      {inactive}
    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Tactical Item                                                      --- */
/* -------------------------------------------------------------------------- */

export interface TacticSelection {
  goal: Goal;
  node: Node;
  locked: boolean;
  selected: Tactic;
  setSelected: (tac: Tactic) => void;
}

interface TacticItemProps extends TAC.tacticalData, TacticSelection {}

function TacticItem(props: TacticItemProps): JSX.Element | null {
  const { id: tactic, status, locked, selected, setSelected } = props;
  if (status === 'NotApplicable') return null;
  const ready = !locked && status === 'Applicable';
  const isSelected = selected === tactic;
  const onSelect = (): void => setSelected(tactic);
  const onPlay = (): void => {
    if (ready) {
      applyTactic(tactic);
      setSelected(undefined);
    }
  };
  const className = classes(
    'dome-color-frame wp-tactical-item',
    isSelected && 'selected',
  );
  return (
    <Hbox
      className={className}
      onClick={onSelect}
      onDoubleClick={onPlay}
    >
      <Item className="wp-tactical-cell" {...props}/>
      <IconButton
        icon={locked ? 'LOCK' : 'MEDIA.PLAY'}
        title='Apply Tactic'
        kind='positive'
        enabled={ready}
        onClick={onPlay} />
    </Hbox>
  );
}

/* -------------------------------------------------------------------------- */
/* --- All Tactics View                                                   --- */
/* -------------------------------------------------------------------------- */

export function Tactics(props: TacticSelection): JSX.Element {
  const { node } = props;
  const tactics = States.useSyncArrayData(TAC.tactical);
  const items =
    node
    ? tactics.map(tac => <TacticItem key={tac.id} {...props} {...tac} />)
    : null;
  return <>{items}</>;
}

/* -------------------------------------------------------------------------- */
/* --- Tactical Parameter                                                 --- */
/* -------------------------------------------------------------------------- */

interface ParameterProps extends TAC.parameter {
  node: TIP.node;
  locked: boolean;
  tactic: TIP.tactic;
}

function CheckBoxParam(props: ParameterProps): JSX.Element
{
  const { id: param, locked, node, tactic, label, title, value } = props;
  const active = value === true;
  const onClick = (): void => {
    if (!locked)
      Server.send(TAC.setParameter, { node, tactic, param, value: !active });
  };
  return (
    <Label
      className="wp-config-checkbox"
      label={label}
      title={title}
      onClick={onClick} >
      <Icon
        className="wp-config-switch"
        size={14}
        offset={-2}
        kind={locked ? 'disabled' : 'default'}
        id={active ? 'SWITCH.ON' : 'SWITCH.OFF'} />
    </Label>
  );
}

function SpinnerParam(props: ParameterProps): JSX.Element
{
  const {
    id: param, locked, node, tactic, label, title,
    vmin, vmax, vstep, value: jval
  } = props;
  const onChange = (value: number): void => {
    if (!locked)
      Server.send(TAC.setParameter, { node, tactic, param, value });
  };
  const value = typeof(jval)==='number' ? jval : undefined;
  return (
    <Label label={label} title={title}>
      <Spinner
        className="wp-config-field wp-config-spinner"
        value={value}
        vmin={vmin}
        vmax={vmax}
        vstep={vstep}
        disabled={locked}
        onChange={onChange}
      />
    </Label>
  );
}

function SelectorParam(props: ParameterProps): JSX.Element
{
  const {
    id: param, locked, node, tactic, label, title,
    value: jval, vlist=[]
  } = props;
  const value = typeof(jval) === 'string' ? jval : undefined;
  const options = vlist.map(({ id, label, title }) =>
    <option key={id} value={id} title={title}>{label}</option>
  );
  const onChange = (value: string | undefined): void => {
    if (!locked)
      Server.send(TAC.setParameter, { node, tactic, param, value });
  };
  return (
    <Label label={label} title={title}>
      <SelectMenu
        className="wp-config-field wp-config-select"
        value={value}
        disabled={locked}
        onChange={onChange}
      >{options}</SelectMenu>
    </Label>
  );
}

function Parameter(props: ParameterProps): JSX.Element | null
{
  if (!props.enabled) return null;
  switch(props.kind) {
    case 'checkbox':
      return <CheckBoxParam {...props} />;
    case 'spinner':
      return <SpinnerParam {...props} />;
    case 'selector':
      return <SelectorParam {...props} />;
    default:
      return null;
  }
}

/* -------------------------------------------------------------------------- */
/* --- Automatic Prover Configuration                                     --- */
/* -------------------------------------------------------------------------- */

export function ConfigureAutoProver(props: ProverSelection): JSX.Element {
  const { node, selected: prover, setSelected } = props;
  const { descr } = States.useSyncArrayElt(WP.ProverInfos, prover) ?? {};
  const result = States.useRequestStable(TIP.getResult, { node, prover });
  const [process = 1, setProcess] = States.useSyncState(WP.process);
  const [timeout = 1, setTimeout] = States.useSyncState(WP.timeout);
  const { icon, kind, clear=true, running=false, play=false, forward=0 } =
    getProverActions(result);
  const [fwdTime, setFwdTime] = React.useState(0);
  const [fwdArmed, setFwdArmed] = React.useState(false);
  const auto = !States.useRequestStable(WP.isInteractiveProver, prover);
  const display = auto && !!prover;
  const action = running ? TIP.killProvers : TIP.runProvers;
  const onPlay = (): void => sendProver(action, node, prover);
  const onClear = (): void => sendProver(TIP.clearProvers, node, prover);
  const onClose = (): void => setSelected(undefined);
  const timedOut = forward > 0 && result.proverTime < timeout;
  const enabled = play || running || timedOut;
  const forwardTitle =
    forward > 0 ? `Run Prover with ${forward}s` : 'Run Prover with Extra Time';
  const onForward = (): void => {
    setFwdTime(forward);
    sendProverTime(node, prover, forward);
  };
  React.useEffect(() => {
    if (fwdTime > 0) {
      if (fwdArmed && !running) {
        setFwdArmed(false);
        setFwdTime(0);
      }
      if (!fwdArmed && running) {
        setFwdArmed(true);
      }
    }
  }, [running, fwdTime, fwdArmed] );
  return (
    <Hbox
      className="dome-xToolBar dome-color-frame wp-configure"
      display={display}
    >
      <Item
        key='prover'
        title='Selected Prover Configuration'
        className="wp-config-tactic"
        label={descr} />
      <Separator />
      <IconButton
        key='clear'
        icon='TRASH'
        kind='negative'
        enabled={clear}
        title='Clear Prover Result'
        onClick={onClear}
        />
      <Item
        icon={icon}
        kind={kind}
        label={result.descr} />
      <Filler />
      <Label
        icon='SETTINGS'
        label='Process' title='Server Processes (shared by all provers)'
      >
        <Spinner
          className="wp-config-field wp-config-spinner"
          onChange={setProcess}
          value={process}
          vmin={1}
          vmax={36}
          vstep={1}
        />
      </Label>
      <Label
        label='Timeout' title='Prover Timeout (shared by all provers)'
      >
        <Spinner
          className="wp-config-field wp-config-spinner"
          enabled={fwdTime===0}
          onChange={setTimeout}
          value={fwdTime > 0 ? fwdTime : timeout}
          vmin={1}
          vmax={3600}
          vstep={1}
        />
      </Label>
      <Separator />
      <Hbox>
        <IconButton
          key='play'
          icon={running ? 'MEDIA.STOP' : 'MEDIA.PLAY'}
          kind={running ? 'warning' : 'positive'}
          title={running ? 'Interrupt Prover' : 'Run Prover'}
          enabled={enabled}
          onClick={onPlay}
        />
        <IconButton
          key='forward'
          icon='MEDIA.NEXT'
          kind='positive'
          enabled={forward > 0}
          title={forwardTitle}
          onClick={onForward}
        />
      </Hbox>
      <IconButton
        key='close'
        icon='CIRC.CLOSE'
        title='Close Prover Configuration Panel'
        onClick={onClose} />
    </Hbox>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Interactive Prover Configuration                                   --- */
/* -------------------------------------------------------------------------- */

export function ConfigureInteractiveProver(sel: ProverSelection): JSX.Element {
  const { node, selected: prover, setSelected } = sel;
  const { descr } = States.useSyncArrayElt(WP.ProverInfos, prover) ?? {};
  const result = States.useRequestStable(TIP.getResult, { node, prover });
  const interactive = States.useRequestStable(WP.isInteractiveProver, prover);
  const [process = 1, setProcess] = States.useSyncState(WP.process);
  const [timeout = 1, setTimeout] = States.useSyncState(WP.timeout);

  const { icon, kind, clear=true, running=false } =
    getProverActions(result);
  const display = interactive && !!prover;
  const kill =
    (): void => sendProver(TIP.killProvers, node, prover);
  const batch =
    (): void => sendProverInteractive(node, prover, WP.InteractiveMode.Batch);
  const edit =
    (): void => sendProverInteractive(node, prover, WP.InteractiveMode.Edit);

  const onClear = (): void => sendProver(TIP.clearProvers, node, prover);
  const onClose = (): void => setSelected(undefined);
  return (
    <Hbox
      className="dome-xToolBar dome-color-frame wp-configure"
      display={display}
    >
      <Item
        key='prover'
        title='Selected Prover Configuration'
        className="wp-config-tactic"
        label={descr} />
      <Separator />
      <IconButton
        key='clear'
        icon='TRASH'
        kind='negative'
        enabled={clear}
        title='Clear Prover Result'
        onClick={onClear}
        />
      <Item
        icon={icon}
        kind={kind}
        label={result.descr} />
      <Filler />
      <Label
        icon='SETTINGS'
        label='Process' title='Server Processes (shared by all provers)'
      >
        <Spinner
          className="wp-config-field wp-config-spinner"
          onChange={setProcess}
          value={process}
          vmin={1}
          vmax={36}
          vstep={1}
        />
      </Label>
      <Label
        label='Timeout' title='Prover Timeout (shared by all provers)'
      >
        <Spinner
          className="wp-config-field wp-config-spinner"
          onChange={setTimeout}
          value={timeout}
          vmin={1}
          vmax={3600}
          vstep={1}
        />
      </Label>
      <Separator/>
      <Hbox>
        <IconButton
          key='play'
          icon={running ? 'MEDIA.STOP' : 'MEDIA.PLAY'}
          title={running ? 'Stop Prover' : 'Run Prover'}
          onClick={running ? kill : batch}
        />
        <IconButton
          key='edit'
          icon={'PAINTBRUSH'}
          title={'Edit Prover Script'}
          enabled={!running}
          onClick={edit}
        />
      </Hbox>
      <IconButton
        key='close'
        icon='CIRC.CLOSE'
        title='Close Prover Configuration Panel'
        onClick={onClose} />
    </Hbox>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Tactical Configuration                                             --- */
/* -------------------------------------------------------------------------- */

const noTactic: TAC.tacticalData = {
  ...TAC.tacticalDataDefault,
  status: 'NotApplicable'
};

function useTactic(selected: Tactic): TAC.tacticalData {
  const data = States.useSyncArrayElt(TAC.tactical, selected);
  return data ?? noTactic;
}

interface StatusDescr {
  icon: string;
  kind: 'warning' | 'positive' | 'default';
  label: string;
}

const Locked: StatusDescr = {
  icon: 'CHECK',
  kind: 'default',
  label: 'Applied',
};

function getStatusDescription(tactical: TAC.tacticalData): StatusDescr {
  const { status, error, params } = tactical;
  if (error)
    return { icon: 'WARNING', kind: 'warning', label: error };
  if (status === 'NotConfigured')
    return { icon: 'WARNING', kind: 'warning', label: 'Missing fields' };
  if (params.length)
    return { icon: 'CHECK', kind: 'positive', label: 'Configured' };
  return { icon: 'CHECK', kind: 'positive', label: 'Ready' };
}

export function ConfigureTactic(props: TacticSelection): JSX.Element {
  const { node, locked, selected: tactic, setSelected } = props;
  const tactical = useTactic(tactic);
  const { pending } = States.useRequestStatus(TAC.configureTactics, node);
  const { status, label, title, params } = tactical;
  const isReady = !locked && status==='Applicable';
  const display = !!tactic && (locked || status !== 'NotApplicable');
  const statusDescr = locked ? Locked : getStatusDescription(tactical);
  const onClose = (): void => setSelected(undefined);
  const onPlay = (): void => { if (isReady) applyTactic(tactic); };
  const onClear = (): void => {
    Server.send(TIP.clearNode, node);
    setSelected(undefined);
  };
  const parameters =
    (node && tactic)
    ? params.map((prm: TAC.parameter) =>
      <Parameter
        key={prm.id}
        node={node}
        tactic={tactic}
        locked={locked}
        {...prm}/>
    ) : null;
  return (
    <Hbox
      className="dome-xToolBar dome-color-frame wp-configure"
      display={display}
    >
      <Item
        key='tactic'
        icon={ pending ? 'EXECUTE' : 'TUNINGS' }
        title='Selected Tactic Configuration'
        className="wp-config-tactic"
        label={label} />
      <Descr
        key='info'
        icon='CIRC.INFO'
        className="wp-config-info"
        label={title} />
      <Filler key='filler'/>
      <Descr
        key='status'
        className="wp-config-info"
        title='Tactic Status'
        {...statusDescr} />
      <Fragment key='params'>{parameters}</Fragment>
      <Separator/>
      <IconButton
        key='play'
        icon={locked ? 'LOCK' : 'MEDIA.PLAY'}
        kind='positive'
        title='Apply Tactic'
        enabled={isReady}
        display={!locked}
        onClick={onPlay} />
      <IconButton
        key='clear'
        icon='MEDIA.PREV'
        kind='negative'
        display={locked}
        title='Cancel Tactic and Remove Sub-Tree'
        onClick={onClear} />
      <IconButton
        key='close'
        icon='CIRC.CLOSE'
        title='Close Tactic Configuration Panel'
        disabled={locked}
        onClick={onClose} />
    </Hbox>
  );
}

/* -------------------------------------------------------------------------- */
