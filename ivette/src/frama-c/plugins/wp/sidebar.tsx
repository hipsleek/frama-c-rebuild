/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import * as Utils from 'dome/misc/utils';
import * as Forms from 'dome/layout/forms';
import { Icon } from 'dome/controls/icons';
import { Label } from 'dome/controls/labels';
import { showHelp } from 'dome/help';
import {
  Checkbox,
  Switch,
  Button,
  SelectMenu,
  Spinner,
  IconButton,
  Field
} from 'dome/controls/buttons';
import { SidebarTitle } from 'dome/frame/sidebars';
import { DivProps, Hbox, Hfill, Space, Vbox } from 'dome/layout/boxes';
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Params from 'frama-c/kernel/api/parameters';
import * as WP from 'frama-c/plugins/wp/api';
import * as TIP from './tip';

/* function search */
import * as Ivette from 'ivette';
import * as Dome from 'dome';
import * as Toolbar from 'dome/frame/toolbars';
import * as Ast from 'frama-c/kernel/api/ast';

/* ************************************************************************** */
/* Generic sidebar block */
/* ************************************************************************** */

interface SidebarBlockProps extends DivProps {
  title?: string;
  titleButtons?: JSX.Element[]; /* to be displayed after title, right aligned */
  foldable?: boolean;
}

function SidebarBlock(props: SidebarBlockProps): JSX.Element {
  const { children, title, titleButtons, foldable = false, ...others } = props;

  const [open, setOpen] = React.useState(!foldable);
  const onClick = (): void => { if (foldable) setOpen(!open); };

  const ftitle = title + (open ? '' : ' ...');
  const ititle = foldable ? (open ? 'ANGLE.DOWN' : 'ANGLE.RIGHT') : undefined;

  return (
    <Vbox className={Utils.classes('wp-sidebar-block')} {...others}>
      <Hbox className={Utils.classes('wp-sidebar-block-title')}>
        <Hbox>
          {title && <Label
            label={ftitle}
            icon={ititle}
            className={Utils.classes('wp-sidebar-block-title-label')}
            onClick={onClick}
          />}
        </Hbox>
        <Hfill />
        <Hbox>
          {titleButtons}
        </Hbox>
      </Hbox>
      {(!foldable || open) && children}
    </Vbox>
  );
}

const makeSidebarHelp = (anchor: string): JSX.Element => {
  const help = (): void => { showHelp(anchor); };
  return (
    <IconButton key='help' icon='HELP' onClick={help} offset={-1} />
  );
};

/* ************************************************************************** */
/* WP general tools */
/* ************************************************************************** */

function Tools(): JSX.Element {
  const { running } = TIP.useServerActivity();
  const goals = States.useSyncArrayProxy(WP.goals).model.getRowCount();

  const run = (): void => { Server.send(WP.startProofs, null); };
  const stop = (): void => { Server.send(WP.cancelProofTasks, null); };
  const clear = (): void => { Server.send(WP.clearProofs, null); };
  const help = (): void => { showHelp('wp'); };
  return (
    <Hbox>
      <Button
        icon='MEDIA.PLAY'
        title='Start WP'
        onClick={run}
        disabled={running}
      />
      <Button
        icon='MEDIA.STOP'
        title='Stop proof tasks'
        onClick={stop}
        enabled={running}
      />
      <Button
        icon='TRASH'
        title='Clear goals'
        onClick={clear}
        disabled={running || goals === 0}
      />
      <Button
        icon='HELP'
        title='WP documentation'
        onClick={help}
      />
    </Hbox>
  );
}

/* ************************************************************************** */
/* Provers section */
/* ************************************************************************** */

/* Prover */
/* ************************************************************************** */

interface ProverConfig {
  prover: WP.prover;
  up: boolean;
  name: string;
  version: string;
}

function Prover(props: ProverConfig): JSX.Element {
  const { prover, up, name, version } = props;
  const [checked, setChecked] = React.useState(up);
  const onChange = (value: boolean): void => {
    setChecked(value);
    Server.send(WP.setProverState, [prover, value]);
  };
  const label = name + ' (' + version + ')';
  return (
    <Switch
      label={label}
      value={checked}
      onChange={onChange}
    />
  );
}

/* Cache Mode */
/* ************************************************************************** */

function CacheSelector(): JSX.Element {
  const [cacheMode, setCacheMode] = States.useSyncState(WP.cacheMode);
  const { help } =
    States.useRequestStable(Params.getParameterInfo, '-wp-cache');
  const onChange = (value: string | undefined): void => {
    const mode =
      value
        ? WP.CacheMode[value as keyof typeof WP.CacheMode]
        : undefined;
    if (mode)
      setCacheMode(mode);
  };
  const options =
    (Object.keys(WP.CacheMode) as Array<keyof typeof WP.CacheMode>)
      .map((value) =>
        <option key={value} value={value}>{value}</option>
      );
  return (
    <>
      <SelectMenu
        value={cacheMode}
        onChange={onChange}
        className="wp-config-field wp-config-select"
      >{options}</SelectMenu>
      <IconButton icon='HELP' title={help} />
    </>
  );
}

/* Interactive Mode */
/* ************************************************************************** */

function InteractiveSelector(): JSX.Element {
  const [inter, setInter] = States.useSyncState(WP.interactiveMode);
  const { help } =
    States.useRequestStable(Params.getParameterInfo, '-wp-interactive');
  const onChange = (value: string | undefined): void => {
    const mode =
      value
        ? WP.InteractiveMode[value as keyof typeof WP.InteractiveMode]
        : undefined;
    if (mode)
      setInter(mode);
  };
  const options =
    (Object.keys(WP.InteractiveMode) as Array<keyof typeof WP.InteractiveMode>)
      .map((value) =>
        <option key={value} value={value}>{value}</option>
      );
  return (
    <Label label='Mode'>
      <SelectMenu
        value={inter}
        onChange={onChange}
        className="wp-config-field wp-config-select"
      >{options}</SelectMenu>
      <IconButton icon='HELP' title={help} />
    </Label>
  );
}

/* TIP Mode */
/* ************************************************************************** */

function TipSelector(): JSX.Element {
  const [tipMode, setTipMode] = States.useSyncState(WP.tipMode);
  const { help } =
    States.useRequestStable(Params.getParameterInfo, '-wp-script');
  const onChange = (value: string | undefined): void => {
    const mode =
      value
        ? WP.TipMode[value as keyof typeof WP.TipMode]
        : undefined;
    if (mode)
      setTipMode(mode);
  };
  const options =
    (Object.keys(WP.TipMode) as Array<keyof typeof WP.TipMode>)
      .map((value) =>
        <option key={value} value={value}>{value}</option>
      );
  return (
    <Label label='Mode'>
      <SelectMenu
        value={tipMode}
        onChange={onChange}
        className="wp-config-field wp-config-select"
      >{options}</SelectMenu>
      <IconButton icon='HELP' title={help} />
    </Label>
  );
}

/* Provers Configuration Component */
/* ************************************************************************** */

function ProversConfiguration(): JSX.Element {
  const goals = States.useSyncArrayProxy(WP.goals).model.getRowCount();

  const [timeout = 0, setTimeout] = States.useSyncState(Params.wpTimeout);
  const [processes = 0, setProcesses] = States.useSyncState(Params.wpPar);
  const [ce = false, setCE] = States.useSyncState(Params.wpCounterExamples);

  const provers = States.useSyncValue(WP.provers) ?? [];
  const proversInfo = States.useSyncArrayGetter(WP.ProverInfos);

  const autoPrvs = provers.filter((p) => proversInfo(p)?.auto);
  const interPrvs = provers.filter((p) => !proversInfo(p)?.auto);

  const [scripts = false, setScripts] = States.useSyncState(WP.scripts);
  const [strategies = false, setStrats] = States.useSyncState(WP.strategies);

  return (
    <Forms.Section label='Provers Configuration' unfold>
      <SidebarBlock
        title='General configuration'
        titleButtons={[makeSidebarHelp('wp-config-provers-general')]}
      >
        <Label label='Timeout' icon='CLOCK' >
          <Spinner
            className="wp-config-field wp-config-spinner"
            value={timeout || 0}
            vmin={0}
            vstep={1}
            onChange={setTimeout}
          />
        </Label>
        <Label label='Processes' icon='SETTINGS'>
          <Spinner
            className="wp-config-field wp-config-spinner"
            value={processes || 0}
            vmin={0}
            vstep={1}
            onChange={setProcesses}
          />
        </Label>
        <Label label='Cache' icon='SERVER'>
          <CacheSelector />
        </Label>
      </SidebarBlock>
      <SidebarBlock
        title='Automatic Provers'
        titleButtons={[makeSidebarHelp('wp-config-provers-auto')]}
      >
        <Checkbox
          label='Generate counter-examples'
          onChange={setCE}
          value={ce}
          enabled={goals === 0}
          title={
            goals === 0
              ? undefined
              : 'Requires to regenerate goals, drop existing results'
          }
        />
        <Space style={{ flexBasis: '8px' }} />
        {
          autoPrvs.length !== 0 ?
            autoPrvs.map((p) =>
              <Prover
                key={p}
                prover={p}
                up={proversInfo(p)?.active ?? false}
                name={proversInfo(p)?.name ?? ''}
                version={proversInfo(p)?.version ?? ''}
              />
            )
            :
            <Label
              label='No automatic provers detected'
              icon='WARNING'
              kind='negative'
            />
        }
      </SidebarBlock>
      <SidebarBlock
        title='Interactive Provers'
        titleButtons={[makeSidebarHelp('wp-config-provers-inter')]}
        display={interPrvs.length !== 0}
      >
        <InteractiveSelector />
        <Space style={{ flexBasis: '8px' }} />
        {
          interPrvs.map((p) =>
            <Prover
              key={p}
              prover={p}
              up={proversInfo(p)?.active ?? false}
              name={proversInfo(p)?.name ?? ''}
              version={proversInfo(p)?.version ?? ''}
            />
          )
        }
      </SidebarBlock>
      <SidebarBlock
        title='No Interactive Provers'
        display={interPrvs.length === 0}
      />
      <SidebarBlock
        title='Proof Strategies'
        titleButtons={[makeSidebarHelp('wp-config-provers-strats')]}
      >
        <TipSelector />
        <Space style={{ flexBasis: '8px' }} />
        <Switch
          label='Use scripts'
          onChange={setScripts}
          value={scripts}
        />
        <Switch
          label='Use strategies'
          onChange={setStrats}
          value={strategies}
        />
      </SidebarBlock>
    </Forms.Section>
  );
}

/* ************************************************************************** */
/* Properties section */
/* ************************************************************************** */

interface SelectionProps {
  name: string;
  selected?: boolean;
  remove: () => void;
}

function SelectionButton(props: SelectionProps): JSX.Element {
  const { name, remove, selected = false } = props;
  const className = Utils.classes(
    'wp-sidebar-selection',
    (selected && 'wp-sidebar-selection-selected')
  );
  return (
    <div className={className}>
      <Label label={name} >
        <Icon
          id='CROSS'
          onClick={remove} />
      </Label>
    </div>
  );
}

/* Properties Filter */
/* ************************************************************************** */

function PropertiesFilter(): JSX.Element {
  const [properties = [], setProperties] = States.useSyncState(WP.filter);

  const [selected, setSelected] = React.useState<string>('');
  const onChange = (value: string | undefined): void => {
    setSelected(value ?? '');
  };

  const [field, setField] = React.useState<string>('');

  const custom = 'Custom:';

  const getName = (add: boolean): string => {
    const name = selected === custom ? field : selected;
    return add ? name : '-' + name;
  };
  const displayName = (name: string): string => {
    switch (name) {
      case '@disjoint_behaviors': return '@disjoint';
      case '@complete_behaviors': return '@complete';
      default: return name;
    }
  };

  const canCommit = (add: boolean): boolean => {
    if (selected === '') return false;
    if (selected === custom && field === '') return false;
    return properties.indexOf(getName(add)) === -1;
  };

  const remove = (ps: string[], value: string): string[] => {
    return ps.filter((element) => element !== value);
  };

  const onCommit = (add: boolean): void => {
    const toRm = getName(!add);
    const toAdd = getName(add);
    const updated =
      properties.indexOf(toRm) !== -1
        ? remove(properties, toRm)
        : [...properties, toAdd];
    setProperties(updated);
  };
  const onKill = (value: string): void => {
    setProperties(remove(properties, value));
  };

  const options = [
    '',
    '@assert',
    '@assigns',
    '@breaks',
    '@check',
    '@continues',
    '@complete_behaviors',
    '@decreases',
    '@disjoint_behaviors',
    '@ensures',
    '@exits',
    '@invariant',
    '@lemma',
    '@requires',
    '@returns',
    '@variant',
    '@terminates',
    custom
  ];

  return (
    <SidebarBlock
      title='Filters'
      titleButtons={[makeSidebarHelp('wp-config-properties-filter')]}
      foldable={true}
    >
      {properties.length !== 0 &&
        <div className={Utils.classes('wp-sidebar-selection-block')}>
          <Button
            key='Reset'
            icon={'CROSS'}
            className={Utils.classes('wp-sidebar-selection-commit')}
            onClick={() => setProperties([])}
          />,
          {properties.map((value) =>
            <SelectionButton
              key={value}
              name={value}
              selected={value === getName(true) || value === getName(false)}
              remove={() => onKill(value)} />)}
        </div>
      }
      <Hbox>
        <Button
          icon={'PLUS'}
          enabled={canCommit(true)}
          onClick={() => { onCommit(true); }}
          className={Utils.classes('wp-sidebar-selection-commit')}
        />
        <Button
          icon={'MINUS'}
          enabled={canCommit(false)}
          onClick={() => { onCommit(false); }}
          className={Utils.classes('wp-sidebar-selection-commit')}
        />
        <SelectMenu
          value={selected}
          onChange={onChange}
        >
          {options.map((value) =>
            <option key={value} value={value}>{displayName(value)}</option>)}
        </SelectMenu>
        <Field
          style={selected !== custom ? { display: 'none' } : {}}
          onEdited={(value) => { setField(value); }}
        />
      </Hbox>
    </SidebarBlock>
  );
}

/* RTE configuration */
/* ************************************************************************** */

/* Simple options */

interface BoolOptionProps {
  state: States.State<boolean>;
  descr: string;
  supported: boolean;
}

function BoolOption(props: BoolOptionProps): JSX.Element {
  const { state, descr, supported } = props;
  const [opt = false, setOpt] = States.useSyncState(state);
  return (
    <Hbox title={ !supported ? 'WP does not support this option' : undefined}>
      <Checkbox
        label={descr}
        onChange={setOpt}
        value={opt}
        enabled={supported}
      />
      {!supported && <Icon id='WARNING' kind='warning'/>}
    </Hbox>
  );
}

/* Special floats */

function SpecialFloats(): JSX.Element {
  const specialFloats: [string, string][] = [
    ['none', 'None'],
    ['nan', 'NaN'],
    ['non-finite', 'Non finite']
  ];

  const mkSF = (entry: [string, string]): JSX.Element => {
    return <option key={entry[0]} value={entry[0]}>{entry[1]}</option>;
  };

  const [sf = 'non-finite', setSF] =
    States.useSyncState(Params.warnSpecialFloat);
  return (
    <Hbox>
      <Checkbox
        key='sf'
        label='Special float '
        style={{ color: 'var(--text-discrete)' }}
        title={
          sf !== 'none'
            ? 'Remove special floats to disable these guards'
            : 'Add special floats to enable these guards'
        }
        value={sf !== 'none'}
      />
      <SelectMenu
        value={sf}
        onChange={(newV) => { if (newV) setSF(newV); }}
        className='wp-config-field wp-config-select'
      >
        {specialFloats.map(mkSF)}
      </SelectMenu>
    </Hbox>
  );
}

/* Initialized */

function globalHints(selected: Ast.decl[]): Ivette.Hint[] {
  const globals = States.getSyncArray(Ast.functions).getArray();
  return globals
    .filter(({ defined }: Ast.functionsData) => defined)
    .filter(({ decl }: Ast.functionsData) => !selected.includes(decl))
    .map((g: Ast.functionsData) => ({
      id: g.decl,
      name: g.name,
      label: g.name,
    }));
}

function lookupHints(hs: Ivette.Hint[], pattern: string): Toolbar.Hint[] {
  const lookupHint = (h: Ivette.Hint, lp: string): boolean => {
    const hn = h.name ?? h.label;
    return hn ? hn.toLowerCase().includes(lp) : false;
  };
  const toHint = (h: Ivette.Hint): Toolbar.Hint => {
    const label = h.label ?? h.name ?? String(h.id);
    return { ...h, label };
  };

  const p = pattern.toLowerCase();
  return hs.filter((h) => lookupHint(h, p)).map(toHint);
}

const focus = new Dome.Event<void>('ivette.wp.functions.focus');

interface SearchFieldProps {
  onHint: (hint: Ivette.Hint) => void;
  alreadySelected: Ast.decl[];
}

/* We want to limit ourselves to the functions */
function SearchField(props: SearchFieldProps): JSX.Element {
  const { onHint, alreadySelected } = props;
  const [pattern, onPattern] = React.useState('');
  const hints = React.useMemo(() => {
    return lookupHints(globalHints(alreadySelected), pattern);
  }, [pattern, alreadySelected]);
  const onSearch = React.useCallback(() => { focus.emit(); }, []);
  return (
    <Toolbar.SearchField
      title='Look for function definition'
      className='wp-sidebar-search'
      placeholder='function name'
      hints={hints}
      onHint={onHint}
      onPattern={onPattern}
      onSearch={onSearch}
      focus={focus}
    />
  );
}

function Initialization(): JSX.Element {
  const [init = { only: true, elems: [] }, setInit] =
    States.useSyncState(WP.initialized);

  const { only, elems } = init;

  const setInitOnly = (value: boolean): void => {
    setInit({ only: value, elems: elems });
  };

  const onHint = (hint: Ivette.Hint): void => {
    const id = hint.id as Ast.decl;
    const name = hint.label ?? "";
    setInit({ only: only, elems: [...elems, [id, name]] });
  };

  const onKill = (value: Ast.decl): void => {
    const newElems = elems.filter(([d, _]) => value !== d);
    setInit({ only: only, elems: newElems });
  };

  const toSelection = ([decl, name]: [Ast.decl, string]): JSX.Element => {
    const remove = (): void => { onKill(decl); };
    return (
      <SelectionButton key={decl} name={name} remove={remove} />
    );
  };
  return (
    <>
      <Checkbox
        key='init'
        label='Initialization '
        style={{ color: 'var(--text-discrete)' }}
        title={
          elems.length !== 0 || !only
            ? 'Clear selection to disable these guards'
            : 'Add functions to the selection to enable these guards'
        }
        value={elems.length !== 0 || !only}
      />
      <Hbox>
        <Button
          label='Only'
          selected={only}
          onClick={(): void => { setInitOnly(true); }}
        />
        <Button
          label='All Except'
          selected={!only}
          onClick={(): void => { setInitOnly(false); }}
        />
        <Button
          label='Clear'
          enabled={elems.length !== 0 || !only}
          onClick={(): void => { setInit({ only: true, elems: [] }); }}
        />
      </Hbox>
      {
        elems.length !== 0
          ? elems.map(toSelection)
          : <Label label={only ? 'No functions' : 'No exceptions'} />
      }
      <SearchField
        onHint={onHint}
        alreadySelected={elems.map(([d, _]) => d)} />
    </>
  );
}

/* RTE Component */

function RTE(): JSX.Element {
  const [rte = false, setRte] = States.useSyncState(Params.wpRte);

  const options: [string, string, boolean, States.State<boolean>][] = [
    /* pointers */
    ["mem", "Invalid memory access", true, Params.rteMem],
    ["pdc", "Pointer downcast", true, Params.warnPointerDowncast],
    ["pointer", "Invalid pointer", true, Params.warnInvalidPointer],
    ["align", "Unaligned pointer", false, Params.warnUnalignedPointer],
    ["ptr-call", "Function pointer calls", false, Params.rtePointerCall],
    /* + initialized, see below */
    /* integers */
    ["Odiv", "Division by 0", true, Params.rteDiv],
    ["sov", "Signed overflow", true, Params.warnSignedOverflow],
    ["uov", "Unsigned overflow", true, Params.warnUnsignedOverflow],
    ["sdc", "Signed downcast", true, Params.warnSignedDowncast],
    ["udc", "Unsigned downcast", true, Params.warnUnsignedDowncast],
    ["shift", "Invalid shift", true, Params.rteShift],
    ["lsn", "Left shift on negative", true, Params.warnLeftShiftNegative],
    ["rsn", "Right shift on negative", true, Params.warnRightShiftNegative],
    /* booleans */
    ["bool", "Invalid bool", true, Params.warnInvalidBool],
    /* floats + special values, see below */
    ["fti", "Invalid float to int", true, Params.rteFloatToInt]
  ];

  const mkOption =
    (entry: [string, string, boolean, States.State<boolean>]): JSX.Element => {
      const [id, descr, supported, state] = entry;
      return (
        <BoolOption
          key={id}
          state={state}
          supported={supported}
          descr={descr} />
      );
    };

  return (
    <SidebarBlock
      title='RTE Guards'
      titleButtons={
        [
          <Checkbox
            key="generate"
            label='Generate'
            value={rte}
            onChange={setRte}
          />,
          makeSidebarHelp('wp-config-properties-rte')
        ]
      }
      foldable={true}
    >
      <Vbox>
        {options.map(mkOption)}
        <SpecialFloats />
        <Initialization />
      </Vbox>
    </SidebarBlock>
  );
}

/* Smoke tests configuration */
/* ************************************************************************** */

function SmokeTests(): JSX.Element {
  const [smoke = false, setSmoke] = States.useSyncState(Params.wpSmokeTests);
  const [assumes, setAssumes] = States.useSyncState(Params.wpSmokeDeadAssumes);
  const [code, setCode] = States.useSyncState(Params.wpSmokeDeadCode);
  const [call, setCall] = States.useSyncState(Params.wpSmokeDeadCall);
  const [loc, setLoc] = States.useSyncState(Params.wpSmokeDeadLocalInit);
  const [loop, setLoop] = States.useSyncState(Params.wpSmokeDeadLoop);

  return (
    <SidebarBlock
      title='Smoke tests'
      titleButtons={
        [
          <Checkbox
            key="generate"
            label='Generate'
            value={smoke}
            onChange={setSmoke}
          />,
          makeSidebarHelp('wp-config-properties-smoke')
        ]
      }
      foldable={true}
    >
      <Vbox>
        <Checkbox label='Assumes' onChange={setAssumes} value={assumes} />
        <Checkbox label='Code' onChange={setCode} value={code} />
        <Checkbox label='Call' onChange={setCall} value={call} />
        <Checkbox label='Local Initialization' onChange={setLoc} value={loc} />
        <Checkbox label='Loop' onChange={setLoop} value={loop} />
      </Vbox>
    </SidebarBlock>
  );
}

/* Properties component */
/* ************************************************************************** */

function Properties(): JSX.Element {
  return (
    <Forms.Section label='Properties' unfold>
      <RTE />
      <SmokeTests />
      <PropertiesFilter />
    </Forms.Section>
  );
}

/* ************************************************************************** */
/* Simplifications section */
/* ************************************************************************** */

interface SimplOptionProps {
  name: string;
  label: string;
  state: States.State<boolean>;
  enabled: boolean;
  message: string | undefined;
}

function SimplOption(props: SimplOptionProps): JSX.Element {
  const { label, name, state, enabled, message } = props;
  const help = States.useRequestStable(Params.getParameterInfo, name).help;
  const [value = false, setValue] = States.useSyncState(state);
  return (
    <Hbox>
      <Checkbox
        label={label}
        onChange={setValue}
        title={message}
        enabled={enabled}
        value={value}
      />
      <IconButton icon='HELP' title={help} />
    </Hbox>
  );
}

function Simplifications(): JSX.Element {
  const goals = States.useSyncArrayProxy(WP.goals).model.getRowCount();

  const [qed = false, setQed] = States.useSyncState(Params.wpQed);
  const [clean = false, setClean] = States.useSyncState(Params.wpClean);

  const simplications: [string, string, States.State<boolean>][] = [
    ['-wp-core', 'Core', Params.wpCore],
    ['-wp-extensional', 'Extensional', Params.wpExtensional],
    ['-wp-filter', 'Filter', Params.wpFilter],
    ['-wp-filter-init', 'Filter Initialization', Params.wpFilterInit],
    ['-wp-ground', 'Ground', Params.wpGround],
    ['-wp-parasite', 'Parasite', Params.wpParasite],
    ['-wp-prenex', 'Prenex-form', Params.wpPrenex],
    ['-wp-pruning', 'Pruning', Params.wpPruning],
    ['-wp-reduce', 'Reduce', Params.wpReduce],
    ['-wp-simplify-forall', 'Forall', Params.wpSimplifyForall],
    ['-wp-simplify-is-cint', 'Redundant int types', Params.wpSimplifyIsCint],
    ['-wp-simplify-land-mask', 'Masks', Params.wpSimplifyLandMask],
    ['-wp-simplify-type', 'Types', Params.wpSimplifyType],
    ['-wp-subst', 'Substitutions', Params.wpSubst],
  ];
  const makeBox =
    (value: [string, string, States.State<boolean>]): JSX.Element => {
      const [name, label, state] = value;

      const message =
        goals !== 0
          ? 'Cannot change simplification parameters when goals exist'
          : !qed
            ? 'Goal simplification is disabled'
            : undefined;
      return (
        <SimplOption
          key={label}
          label={label}
          name={name}
          state={state}
          enabled={qed && goals === 0}
          message={message}
        />
      );
    };

  const globalMessage =
    goals !== 0
      ? 'Cannot change simplification parameters when goals exist'
      : undefined;

  const cleanMessage =
    goals !== 0
      ? 'Cannot change simplification parameters when goals exist'
      : qed
        ? 'Clean only usable when goal simplifiation is disabled'
        : undefined;

  return (
    <Forms.Section label='Sequent Simplifications'>
      <SidebarBlock
        title='Simplifications'
        titleButtons={
          [
            <Checkbox
              key='Enabled'
              label='Enabled'
              title={globalMessage}
              value={qed}
              enabled={goals === 0}
              onChange={setQed}
            />,
            makeSidebarHelp('wp-config-simpl')
          ]
        }
      >
        <Checkbox
          label='Clean'
          onChange={setClean}
          title={cleanMessage}
          enabled={!qed && goals === 0}
          value={clean}
        />
        {simplications.map(makeBox)}
      </SidebarBlock>
    </Forms.Section>
  );
}

/* ************************************************************************** */
/* SideBar */
/* ************************************************************************** */


export function SideBar(): JSX.Element {
  return (
    <>
      <SidebarTitle label='Weakest Precondition'>
        <Tools />
      </SidebarTitle>
      <Forms.SidebarForm className={Utils.classes('wp-sidebar')}>
        <ProversConfiguration />
        <Properties />
        <Simplifications />
      </Forms.SidebarForm>
    </>
  );
}
