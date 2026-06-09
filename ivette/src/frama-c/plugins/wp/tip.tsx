/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';

import * as Dome from 'dome';

import { classes } from 'dome/misc/utils';
import { Icon } from 'dome/controls/icons';
import { Cell, Item, Label, IconKind } from 'dome/controls/labels';
import { IconButton } from 'dome/controls/buttons';
import { TextProxy, TextView, Decoration } from 'dome/text/richtext';
import {
  ToolBar, Select, Filler,
  Button, ButtonGroup
} from 'dome/frame/toolbars';
import { Hfill, Vfill, Vbox, Overlay, Hbox } from 'dome/layout/boxes';
import { writeClipboardText } from 'dome/system';

import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as WP from 'frama-c/plugins/wp/api';
import * as TIP from 'frama-c/plugins/wp/api/tip';

import { getStatus } from './goals';
import { GoalView } from './seq';
import {
  Provers,
  Tactics,
  ConfigureAutoProver,
  ConfigureInteractiveProver,
  ConfigureTactic
} from './tac';

import * as WpStrategy from 'frama-c/plugins/wp/api/strategydebugger';

/* -------------------------------------------------------------------------- */
/* --- Sequent Printing Modes                                             --- */
/* -------------------------------------------------------------------------- */

type Node = TIP.node | undefined;
type Prover = WP.prover | undefined;
type Tactic = TIP.tactic | undefined;

interface Selector<A> {
  value: A;
  setValue: (value: A) => void;
}

interface BoolSelector extends Selector<boolean> {
  label: string;
  title: string;
  disabled?: boolean;
}

function AFormatSelector(props: BoolSelector): JSX.Element {
  const { value, setValue, disabled } = props;
  const className = classes(
    'wp-printer-field wp-printer-button',
    value && 'selected'
  );
  return (
    <Item
      className={className}
      label={props.label}
      title={props.title}
      onClick={(): void => { if (!disabled) { setValue(!value); } }}
    >
      <Icon
        id={value ? 'SWITCH.ON' : 'SWITCH.OFF'}
        kind={disabled ? 'disabled' : 'default'}
      />
    </Item>
  );
}

function IFormatSelector(props: Selector<TIP.iformat>): JSX.Element {
  const { value, setValue } = props;
  return (
    <Select
      className='wp-printer-field wp-printer-select'
      value={value}
      title='Large integers format.'
      onChange={(v) => setValue(TIP.jIformat(v))}
    >
      <option value='dec' title='Integer'>int</option>
      <option value='hex' title='Hex.'>hex</option>
      <option value='bin' title='Binary'>bin</option>
    </Select>
  );
}

function RFormatSelector(props: Selector<TIP.rformat>): JSX.Element {
  const { value, setValue } = props;
  return (
    <Select
      className='wp-printer-field wp-printer-select'
      value={value}
      title='Floatting point format.'
      onChange={(v) => setValue(TIP.jRformat(v))}
    >
      <option value='ratio' title='Rational fraction'>frac</option>
      <option value='float' title='IEEE Float 32-bits'>f32</option>
      <option value='double' title='IEEE Float 64-bits'>f64</option>
    </Select>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Node Path                                                          --- */
/* -------------------------------------------------------------------------- */

interface NodeProps {
  node: TIP.node;
  parent?: boolean;
  child?: boolean;
  hasChildren?: boolean;
}

function Node(props: NodeProps): JSX.Element {
  const cellRef = React.useRef<HTMLLabelElement>(null);
  const {
    node,
    parent = false,
    child = false,
    hasChildren = false,
  } = props;
  const {
    title, childLabel = 'Script', header,
    proved = false, pending = 0, size = 0
  } = States.useRequestStable(TIP.getNodeInfos, node);
  const elt = cellRef.current;
  const current = !child && !parent;
  React.useEffect(() => {
    if (current && elt) elt.scrollIntoView();
  }, [elt, current]);
  const className = classes(
    'wp-navbar-node',
    parent && 'parent',
    child && 'child',
    current && 'current',
  );
  const icon =
    parent ? 'ANGLE.DOWN' :
      child ? 'ANGLE.RIGHT' :
        hasChildren ? 'TRIANGLE.RIGHT' : 'TRIANGLE.RIGHT';
  const kind = proved ? 'positive' : (parent ? 'default' : 'warning');
  const nodeName = header ? `${childLabel}: ${header}` : childLabel;
  const nodeRest = size <= 1 ? '?' : `${pending}/${size}`;
  const nodeFull = size <= 1 ? nodeName : `${nodeName} (${size})`;
  const nodeLabel = proved ? nodeFull : `${nodeName} (${nodeRest})`;
  const proofState =
    proved ? 'proved' :
      pending < size ? `pending ${pending}/${size}` : 'unproved';
  const fullTitle = `${title} (${proofState})`;
  const onSelection = (): void => { Server.send(TIP.goToNode, node); };
  return (
    <Item
      ref={cellRef} className={className}
      icon={icon} kind={kind}
      label={nodeLabel}
      title={fullTitle}
      onClick={onSelection}
    />
  );
}

interface NavBarProps {
  current: TIP.node | undefined;
  parents: TIP.node[];
  subgoals: TIP.node[];
}

function NavBar(props: NavBarProps): JSX.Element {
  const parents = props.parents.map(n => (
    <Node key={n} node={n} parent />
  )).reverse();
  const children = props.subgoals.map(n => (
    <Node key={n} node={n} child />
  ));
  const hasChildren = children.length > 0;
  const current = (n => n ?
    <Node key={n} node={n} hasChildren={hasChildren} />
    : undefined
  )(props.current);
  return (
    <Vbox className='wp-navbar'>
      <Vbox>
        {parents}
        {current}
        {children}
      </Vbox>
    </Vbox>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Active Tasks                                                       --- */
/* -------------------------------------------------------------------------- */

export interface ServerActivity {
  procs: number,
  active: number,
  done: number,
  todo: number,
  running: boolean,
}

export function useServerActivity(): ServerActivity {
  const { procs, active, done, todo } =
    States.useRequestStable(WP.getScheduledTasks, null);
  const running = active + todo > 0;
  return { procs, active, done, todo, running };
}

export function cancelProofTasks(): void {
  Server.send(WP.cancelProofTasks, null);
}

/* -------------------------------------------------------------------------- */
/* --- Pattern Debugger                                                   --- */
/* -------------------------------------------------------------------------- */

const severityIcon: { [key: string]: string } = {
  Ok: 'CHECK',
  Ignored: 'CHECK',
  Warning: 'WARNING',
  Error: 'CROSS',

};

const severityKind: { [key: string]: IconKind } = {
  Ok: 'positive',
  Ignored: 'default',
  Warning: 'warning',
  Error: 'negative',
};

const severityClass: { [key: string]: string } = {
  Ok: 'wp-pattern-ok',
  Ignored: 'wp-pattern-ignored',
  Warning: 'wp-pattern-warning',
  Error: 'wp-pattern-error',
};

const selectedSeverityClass: { [key: string]: string } = {
  Ok: 'wp-pattern-selected-ok',
  Ignored: 'wp-pattern-selected-ignored',
  Warning: 'wp-pattern-selected-warning',
  Error: 'wp-pattern-selected-error',
};

interface FieldProps {
  node?: TIP.node;
  field: WpStrategy.field
}

function Field(props: FieldProps): JSX.Element {
  const node = props.node;
  const { label, title, value, debug, target } = props.field;
  const { part, term } = target;
  const selectable = node && term;
  const icon = selectable ? 'TARGET' : 'TERMINAL';
  const className = selectable && 'wp-select-pattern';
  const onClick = selectable && (() => {
    Server.send(TIP.setSelection, { node, part, term });
  });
  return (
    <tr>
      <td className={className}>
        <Item icon={icon} label={label} title={title} onClick={onClick} />
      </td>
      <td style={{ width: '100%' }}>
        <Cell label={value} title={debug} />
      </td>
    </tr>
  );
}

interface DebugProps {
  node?: TIP.node;
  fields?: WpStrategy.field[];
}

function Fields(props: DebugProps): JSX.Element | null {
  const { node, fields = [] } = props;
  if (!fields) return null;
  return (
    <>
      <table>
        <tbody>
          {fields.map((p) => <Field key={p.label} node={node} field={p} />)}
        </tbody>
      </table>
    </>
  );
}

function decorateDiagnostic(
  buffer: Decoration[], diag: WpStrategy.diagnostic, selected: boolean
): void {
  const { severity, range } = diag;
  if (range)
    buffer.push({ className: severityClass[severity], ...range });
  if (range && selected)
    buffer.push({ className: selectedSeverityClass[severity], ...range });
}

function decorateAlternatives(
  buffer: Decoration[], alt: WpStrategy.alternative[], selected: number
): void {
  alt.forEach((a, k) => {
    const sel = (k === selected);
    a.diagnostics.forEach((d) => decorateDiagnostic(buffer, d, sel));
  });
}

function decorateSelection(
  buffer: Decoration[], selRange: WpStrategy.range | undefined
): void {
  if (selRange)
    buffer.push({ className: 'wp-pattern-selected', ...selRange });
}

function Diagnostic(props: WpStrategy.diagnostic): JSX.Element {
  const { severity, message } = props;
  const icon = severityIcon[severity];
  const kind = severityKind[severity];
  return <Label icon={icon} kind={kind} label={message} />;
}

interface ErrorsProps { errors: WpStrategy.diagnostic[] }

function Errors(props: ErrorsProps): JSX.Element {
  return (
    <Vbox>
      {props.errors.map((d, k) => <Diagnostic key={k} {...d} />)}
    </Vbox>
  );
}

export interface PatternDebuggerProps {
  goal: WP.goal | undefined;
}

export function StrategyDebugger(props: PatternDebuggerProps): JSX.Element {
  const goal = props.goal;
  const status =
    States.useRequestStable(
      TIP.getProofStatus,
      goal ? { main: goal } : undefined
    );

  const current = goal ? status.current : undefined;
  const text = React.useMemo(() => new TextProxy(), []);
  const [strategy, setStrategy] = React.useState('');
  const [selected, setSelected] = React.useState(0);

  const onChange = React.useCallback(() => {
    setStrategy(text.toString());
    setSelected(0);
  }, [text]);

  const alternatives =
    States.useRequestStable(WpStrategy.debug, { strategy, node: current });

  const alternative = alternatives[selected];
  const errors = alternative?.diagnostics || [];
  const fields = alternative?.fields || [];
  const range = alternative?.location;
  const decorations = React.useMemo(() => {
    const buffer: Decoration[] = [];
    decorateSelection(buffer, range);
    decorateAlternatives(buffer, alternatives, selected);
    return buffer;
  }, [alternatives, range, selected]);

  return (
    <Vbox style={{ height: '100%' }}>
      <TextView
        style={{ flex: 1 }}
        text={text}
        onChange={onChange}
        decorations={decorations}
      />
      <Vbox style={{ flex: 1 }}>
        <Label
          display={!alternatives.length}
          label={strategy === '' ? "No strategy" : "No selection"}
        />
        <Hbox display={alternatives.length > 1} className="labview-titlebar" >
          <IconButton
            icon={'ANGLE.LEFT'}
            enabled={selected > 0}
            onClick={() => { setSelected(selected - 1); }}
          />
          <IconButton
            icon={'ANGLE.RIGHT'}
            enabled={selected < alternatives.length - 1}
            onClick={() => { setSelected(selected + 1); }}
          />
          <Label>Alternative #{selected + 1} / {alternatives.length} </Label>
        </Hbox>
        <Errors errors={errors} />
        <Fields node={current} fields={fields} />
      </Vbox>
    </Vbox>
  );
}

/* -------------------------------------------------------------------------- */
/* --- TIP View                                                           --- */
/* -------------------------------------------------------------------------- */

export interface TIPProps {
  display: boolean;
  goal: WP.goal | undefined;
  onClose: () => void;
}

export function TIPView(props: TIPProps): JSX.Element {
  const { display, goal, onClose } = props;
  // --- navbar settings
  const [unproved, flipU] = Dome.useFlipSettings('frama-c.wp.goals.unproved');
  const [subtree, flipT] = Dome.useFlipSettings('frama-c.wp.goals.subtree');
  // --- current goal
  const infos =
    States.useSyncArrayElt(WP.goals, goal) ?? WP.goalsDataDefault;
  // --- proof status
  const {
    current, index, pending, size,
    tactic: nodeTactic, parents = [], children = [],
  } = States.useRequestStable(
    TIP.getProofStatus,
    goal ? { main: goal, unproved, subtree } : undefined,
  );
  // --- script status
  const { saved, proof, script } =
    States.useRequestStable(TIP.getScriptStatus, goal);
  // --- provers
  const server = useServerActivity();
  // --- sidebar & toolbar states
  const [copied, setCopied] = React.useState(false);
  const [tactic, setTactic] = React.useState<Tactic>();
  const [prover, setProver] = React.useState<Prover>();
  // --- printer settings
  const [autofocus, setAF] = Dome.useBoolSettings('wp.tip.autofocus', true);
  const [memory, setMEM] = Dome.useBoolSettings('wp.tip.unmangled', true);

  const [enabledCE, _] = States.useSyncState(WP.counterExamples);
  const [showCE, setSCE] = React.useState(enabledCE ?? false);
  const ceTitle =
    'Show counter examples.' +
    (!enabledCE ? ' (disabled, use -wp-counter-examples)' : '');

  const [iformat, setIformat] = Dome.useWindowSettings<TIP.iformat>(
    'wp.tip.iformat', TIP.jIformat, 'dec'
  );
  const [rformat, setRformat] = Dome.useWindowSettings<TIP.rformat>(
    'wp.tip.rformat', TIP.jRformat, 'ratio'
  );

  // --- Script buttons
  const onSave = (): void => { Server.send(TIP.saveScript, goal); };
  const onReload = (): void => { Server.send(TIP.runScript, goal); };
  const onTrash = (): void => { Server.send(TIP.clearProofScript, goal); };
  const onCopied = Dome.useDebounced(() => setCopied(false), 1000);
  const copyTitle = copied ? 'Copied' : `Script: ${script} (Click to copy)`;
  const onCopy = (): void => {
    if (script) writeClipboardText(script);
    setCopied(true);
    onCopied();
  };

  // --- derived states
  const locked = nodeTactic !== undefined;
  const configuredTactic = nodeTactic ?? tactic;

  // --- current prover
  const onSelectedProver = React.useCallback((prv: Prover) => {
    setTactic(undefined);
    setProver(prv);
  }, []);

  // --- current tactic
  const onSelectedTactic = React.useCallback((tac: Tactic) => {
    setTactic(tac);
    setProver(undefined);
  }, []);

  // --- Delete button
  const deleteAble = locked || children.length > 0;
  const onDelete = (): void => {
    const request = locked ? TIP.clearNodeTactic : TIP.clearParentTactic;
    Server.send(request, current);
  };
  const deleteTitle = locked ? 'Cancel Node Tactic' : 'Cancel Last Tactic';

  // --- Next button
  const nextIndex =
    pending <= 0 ? undefined :
      pending === 1 ? (index === 0 ? undefined : 0) :
        (index + 1 < pending ? index + 1 : 0);
  const prevIndex =
    pending <= 0 ? undefined :
      pending === 1 ? (index === 0 ? undefined : 0) :
        (index < 1 ? pending - 1 : index - 1);
  const nextAble = goal !== undefined && nextIndex !== undefined;
  const prevAble = goal !== undefined && prevIndex !== undefined;
  const nextTitle =
    nextIndex !== undefined
      ? `Goto Next Pending Goal (#${nextIndex + 1} / ${pending})`
      : `Goto Next Pending Goal (if any)`;
  const prevTitle =
    prevIndex !== undefined
      ? `Goto Previous Pending Goal (#${prevIndex + 1} / ${pending})`
      : `Goto Previous Pending Goal (if any)`;
  const onNext = (): void => {
    Server.send(TIP.goToIndex, [goal, nextIndex]);
  };
  const onPrev = (): void => {
    Server.send(TIP.goToIndex, [goal, prevIndex]);
  };

  // --- Component
  return (
    <Vfill display={display}>
      <ToolBar>
        <Cell icon='HOME' label={infos.wpo} title='Goal identifier' />
        <Item
          icon='CODE'
          display={0 <= index && index < pending && 1 < pending}
          label={`${index + 1}/${pending}`} title='Pending proof nodes' />
        <Item {...getStatus(infos)} />
        <IconButton
          display={size > 1}
          icon='CIRC.PLUS' selected={subtree} onClick={flipT}
          title='Show all tactic nodes' />
        <IconButton
          display={size > 1}
          icon='CIRC.CHECK' selected={unproved} onClick={flipU}
          title='Show unproved sub-goals only' />
        <Filler />
        <IconButton
          icon={copied ? 'DUPLICATE' : (saved ? 'FOLDER' : 'FOLDER.OPEN')}
          visible={script !== undefined}
          title={copyTitle}
          onClick={onCopy} />
        <ButtonGroup>
          <Button
            icon='RELOAD'
            enabled={script !== undefined && !saved}
            title='Replay Proof Script from Disk'
            onClick={onReload}
          />
          <Button
            icon='SAVE'
            enabled={proof && !saved}
            title='Save Proof Script on Disk'
            onClick={onSave}
          />
        </ButtonGroup>
        <ButtonGroup>
          <Button
            icon='MEDIA.PREV'
            kind='negative'
            enabled={deleteAble}
            title={deleteTitle}
            onClick={onDelete} />
          <Button
            icon='CROSS'
            kind='negative'
            enabled={proof || script !== undefined}
            title='Clear Proof and Remove Script (if any)'
            onClick={onTrash}
          />
        </ButtonGroup>
        <ButtonGroup>
          <Button
            icon='ANGLE.LEFT'
            enabled={prevAble}
            title={prevTitle}
            onClick={onPrev} />
          <Button
            icon='EJECT'
            title='Close Proof Transformer (back to goals)'
            onClick={onClose} />
          <Button
            icon='ANGLE.RIGHT'
            enabled={nextAble}
            title={nextTitle}
            onClick={onNext} />
        </ButtonGroup>
        <Button
          icon='MEDIA.HALT'
          kind='negative'
          enabled={server.running}
          title={`Interrupt (${server.todo}/${server.todo + server.done})`}
          onClick={cancelProofTasks}
        />
      </ToolBar>
      <Hfill>
        <NavBar
          current={current}
          parents={parents}
          subgoals={children}
        />
        <Vfill className='dome-positioned'>
          <Overlay display className='wp-printer'>
            <Icon
              id='LOCK'
              title='Tactical Selection Locked'
              className='wp-printer-locked'
              display={locked} />
            <AFormatSelector
              value={showCE} setValue={setSCE} disabled={!enabledCE}
              label='CE' title={ceTitle} />
            <AFormatSelector
              value={autofocus} setValue={setAF}
              label='AF' title='Autofocus mode.' />
            <AFormatSelector
              value={memory} setValue={setMEM}
              label='MEM' title='Memory model internals.' />
            <IFormatSelector value={iformat} setValue={setIformat} />
            <RFormatSelector value={rformat} setValue={setRformat} />
          </Overlay>
          <GoalView
            node={current}
            locked={locked}
            showce={showCE}
            autofocus={autofocus}
            unmangled={!memory}
            iformat={iformat}
            rformat={rformat}
          />
        </Vfill>
        <Vbox className='wp-sidebar-view dome-color-frame'>
          <Provers
            node={current}
            selected={prover}
            setSelected={onSelectedProver}
          />
          <Tactics
            goal={goal}
            node={current}
            locked={locked}
            selected={configuredTactic}
            setSelected={onSelectedTactic}
          />
        </Vbox>
      </Hfill>
      <ConfigureAutoProver
        node={current}
        selected={prover}
        setSelected={onSelectedProver}
      />
      <ConfigureInteractiveProver
        node={current}
        selected={prover}
        setSelected={onSelectedProver}
      />
      <ConfigureTactic
        goal={goal}
        node={current}
        locked={locked}
        selected={configuredTactic}
        setSelected={onSelectedTactic}
      />
    </Vfill>

  );
}

/* -------------------------------------------------------------------------- */
