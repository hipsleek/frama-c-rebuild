/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Properties
// --------------------------------------------------------------------------

import _ from 'lodash';
import React from 'react';

import * as Dome from 'dome';
import * as Json from 'dome/data/json';
import * as Compare from 'dome/data/compare';
import * as Settings from 'dome/data/settings';
import { Label, Code } from 'dome/controls/labels';
import { Icon } from 'dome/controls/icons';
import { IconButton, Checkbox } from 'dome/controls/buttons';
import * as Form from 'dome/layout/forms';
import * as Models from 'dome/table/models';
import * as Arrays from 'dome/table/arrays';
import { Scroll } from 'dome/layout/boxes';
import { Section } from 'dome/frame/sidebars';
import { Table, Column, ColumnProps, Renderer } from 'dome/table/views';
import { RSplit } from 'dome/layout/splitters';

import { TitleBar } from 'ivette';
import { menuItem, setting } from './Globals';

import * as Ast from 'frama-c/kernel/api/ast';
import * as EvaAst from 'frama-c/plugins/eva/api/ast';
import { taintStatus, taintStatusTags } from 'frama-c/plugins/eva/api/taint';
import * as Properties from 'frama-c/kernel/api/properties';
import * as States from 'frama-c/states';


type PropKey = Json.key<'#marker'>;
type Property = Properties.statusData |
  (Properties.statusData & EvaAst.propertiesData);

// --------------------------------------------------------------------------
// --- Filters
// --------------------------------------------------------------------------
export enum EFilterType {
  STATUS = "status",
  KIND = "kind",
  SOURCE = "source",
  ALARMS = "alarms",
  EVA = "eva"
}
export type TFilterType = `${EFilterType}`;

interface IFilterContent {
  value: boolean;
  label: string;
  title?: string;
}

function newFilter(
  value: boolean,
  label: string,
  title?: string,
): IFilterContent {
  return { value, label, title };
}

const DEFAULTS: { [key: string]: IFilterContent } = {
  'currentScope': newFilter(false, "Current scope"),
  /** source */
  'source.alarms': newFilter(true, "Alarms"),
  'source.libc': newFilter(true, "Libc specifications"),
  'source.others': newFilter(true, "Others"),
  /** status */
  'status.valid': newFilter(true, "Valid"),
  'status.valid_hyp': newFilter(true, "Valid under hyp."),
  'status.unknown': newFilter(true, "Unknown"),
  'status.invalid': newFilter(true, "Invalid"),
  'status.invalid_hyp': newFilter(true, "Invalid under hyp."),
  'status.considered_valid': newFilter(false, "Considered valid"),
  'status.untried': newFilter(false, "Untried"),
  'status.dead': newFilter(false, "Dead"),
  'status.inconsistent': newFilter(true, "Inconsistent"),
  /** kind */
  'kind.assert': newFilter(true, "Assertions"),
  'kind.invariant': newFilter(true, "Invariants"),
  'kind.variant': newFilter(true, "Variants"),
  'kind.requires': newFilter(true, "Preconditions"),
  'kind.ensures': newFilter(true, "Postconditions"),
  'kind.instance': newFilter(true, "Instance"),
  'kind.assigns': newFilter(true, "Assigns clauses"),
  'kind.froms': newFilter(true, "From clauses"),
  'kind.allocates': newFilter(true, "Allocates"),
  'kind.behavior': newFilter(false, "Behaviors"),
  'kind.reachable': newFilter(false, "Reachables"),
  'kind.axiomatic': newFilter(true, "Axiomatics"),
  'kind.pragma': newFilter(true, "Pragma"),
  'kind.assumes': newFilter(true, "Assumes"),
  'kind.others': newFilter(true, "Others"),
  /** alarms */
  'alarms.overflow': newFilter(true, "Overflows"),
  'alarms.division_by_zero': newFilter(true, "Divisions by zero"),
  'alarms.shift': newFilter(true, "Shifts"),
  'alarms.special_float': newFilter(true, "Special floats"),
  'alarms.float_to_int': newFilter(true, "Float to int"),
  'alarms.bool_value': newFilter(true, "_Bool values"),
  'alarms.mem_access': newFilter(true, "Memory accesses"),
  'alarms.index_bound': newFilter(true, "Index bounds"),
  'alarms.pointer_alignment': newFilter(true, "Pointers alignment"),
  'alarms.initialization': newFilter(true, "Initializations"),
  'alarms.dangling_pointer': newFilter(true, "Dangling pointers"),
  'alarms.pointer_value': newFilter(true, "Pointer values"),
  'alarms.function_pointer': newFilter(true, "Function pointers"),
  'alarms.ptr_comparison': newFilter(true, "Pointer comparisons"),
  'alarms.differing_blocks': newFilter(true, "Differing blocks"),
  'alarms.separation': newFilter(true, "Separations"),
  'alarms.overlap': newFilter(true, "Overlaps"),
  /** eva */
  'eva.priority_only': newFilter(false, "High-priority only",
    "Show only high-priority properties for the Eva analysis"
  ),
  'eva.data_tainted_only': newFilter(false, "Data-tainted only",
    "Show only data-tainted properties according to the Eva taint domain"
  ),
  'eva.ctrl_tainted_only': newFilter(false, "Control-tainted only",
    "Show only control-tainted properties according to the Eva taint domain"
  ),
};

function filter(path: string): boolean {
  const defaultValue = DEFAULTS[path] ?? true;
  return Settings.getWindowSettings(
    `ivette.properties.filter.${path}`,
    Json.jBoolean,
    defaultValue.value,
  );
}

export function useFilter(path: string): [boolean, () => void] {
  const defaultValue = DEFAULTS[path] ?? true;
  return Dome.useFlipSettings(
    `ivette.properties.filter.${path}`,
    defaultValue.value,
  );
}

function useFilterStr(path: string): Form.FieldState<string> {
  const [value, setValue] = Dome.useStringSettings(
    `ivette.properties.filter.${path}`
  );
  const [error, setError] = React.useState<Form.FieldError>(undefined);
  const onChanged = React.useCallback(
    (newValue: string, newError: Form.FieldError) => {
      setValue(newValue);
      setError(newError);
      if (Form.isValid(newError)) Reload.emit();
    }, [setValue],
  );
  return { value, error, onChanged };
}

function resetFilters(prefix: string, b?: boolean): void {
  for (const key in DEFAULTS) {
    if (key.startsWith(prefix)) {
      const target = b ?? DEFAULTS[key].value;
      const path = `ivette.properties.filter.${key}`;
      Settings.setWindowSettings(path, target);
    }
  }
  Reload.emit();
}

function filterSummary(prefix: string): string {
  let total = 0;
  let enabled = 0;
  for (const key in DEFAULTS) {
    if (key.startsWith(prefix)) {
      total++;
      if (filter(key)) enabled++;
    }
  }
  if (enabled === 0) return 'none';
  if (enabled === total) return 'all';
  return `${enabled} / ${total}`;
}

function filterStatus(
  status: Properties.propStatus,
): boolean {
  switch (status) {
    case 'valid':
      return filter('status.valid');
    case 'valid_under_hyp':
      return filter('status.valid_hyp');
    case 'invalid':
      return filter('status.invalid');
    case 'invalid_under_hyp':
      return filter('status.invalid_hyp');
    case 'inconsistent':
      return filter('status.inconsistent');
    case 'unknown':
      return filter('status.unknown');
    case 'considered_valid':
      return filter('status.considered_valid');
    case 'never_tried':
      return filter('status.untried');
    case 'valid_but_dead':
    case 'unknown_but_dead':
    case 'invalid_but_dead':
      return filter('status.dead');
    default:
      return true;
  }
}

function filterKind(
  kind: Properties.propKind,
): boolean {
  switch (kind) {
    case 'assert': return filter('kind.assert');
    case 'loop_invariant': return filter('kind.invariant');
    case 'loop_variant': return filter('kind.variant');
    case 'requires': return filter('kind.requires');
    case 'ensures': return filter('kind.ensures');
    case 'instance': return filter('kind.instance');
    case 'assigns': return filter('kind.assigns');
    case 'froms': return filter('kind.froms');
    case 'allocates': return filter('kind.allocates');
    case 'behavior': return filter('kind.behavior');
    case 'reachable': return filter('kind.reachable');
    case 'axiomatic': return filter('kind.axiomatic');
    case 'assumes': return filter('kind.assumes');
    default: return filter('kind.others');
  }
}

function filterSource(p: Property): boolean {
  const condAlarms = Boolean(p.alarm);
  const condLibc = p.from_libc;
  const others = Boolean(!condAlarms && !condLibc);

  return (filter('source.libc') || !condLibc)
    && (filter('source.alarms') || !condAlarms)
    && (filter('source.others') || !others);
}

function filterAlarm(alarm: string | undefined): boolean {
  switch (alarm) {
    case 'overflow': return filter('alarms.overflow');
    case 'division_by_zero': return filter('alarms.division_by_zero');
    case 'mem_access': return filter('alarms.mem_access');
    case 'index_bound': return filter('alarms.index_bound');
    case 'pointer_alignment': return filter('alarms.pointer_alignment');
    case 'pointer_value': return filter('alarms.pointer_value');
    case 'shift': return filter('alarms.shift');
    case 'ptr_comparison': return filter('alarms.ptr_comparison');
    case 'differing_blocks': return filter('alarms.differing_blocks');
    case 'separation': return filter('alarms.separation');
    case 'overlap': return filter('alarms.overlap');
    case 'initialization': return filter('alarms.initialization');
    case 'dangling_pointer': return filter('alarms.dangling_pointer');
    case 'is_nan_or_infinite':
    case 'is_nan': return filter('alarms.special_float');
    case 'float_to_int': return filter('alarms.float_to_int');
    case 'function_pointer': return filter('alarms.function_pointer');
    case 'bool_value': return filter('alarms.bool_value');
    default: return true;
  }
}

function filterEva(p: Property): boolean {
  if ('priority' in p && p.priority === false && filter('eva.priority_only'))
    return false;
  if ('taint' in p) {
    const taint = p.taint as taintStatus;
    switch (taint) {
      case taintStatus.not_tainted:
      case taintStatus.not_applicable:
        return !filter('eva.data_tainted_only') &&
          !filter('eva.ctrl_tainted_only');
      case taintStatus.direct_taint:
        return !(filter('eva.ctrl_tainted_only'));
      case taintStatus.indirect_taint:
        return !(filter('eva.data_tainted_only'));
      case taintStatus.not_computed:
      case taintStatus.error:
        break;
      default:
        throw taint satisfies never;
    }
  }
  return true;
}

function filterNames(names: string[]): boolean {
  const field = Settings.getWindowSettings(
    `ivette.properties.filter.names`,
    Json.jString,
    "",
  );
  if (!field || field.length < 2) return true;
  const strNames = names.join(':');
  const regex = new RegExp(field, 'i');
  return regex.test(strNames);
}

export function filterProperty(p: Property): boolean {
  return filterStatus(p.status)
    && filterKind(p.kind)
    && filterAlarm(p.alarm)
    && filterEva(p)
    && filterNames(p.names);
}

export interface IFilterState {
  contextMenu: Dome.PopupMenuItem[],
  show: (status: Properties.statusData) => boolean
}

function getContextMenu(
  type: TFilterType,
  filters: { [key: string]: setting }
): Dome.PopupMenuItem[] {
  const typeStr = type + ".";
  return Object.entries(DEFAULTS)
    .filter(([k,]) => k.startsWith(typeStr))
    .map(([k, elt]) => menuItem({
      label: elt.label,
      state: filters[k.replace(typeStr, "")]
    }));
}

export function useStatusFilter(): IFilterState {
  const filters: { [key: string]: setting } = {};

  filters['valid'] = useFilter('status.valid');
  filters['valid_hyp'] = useFilter('status.valid_hyp');
  filters['unknown'] = useFilter('status.unknown');
  filters['invalid'] = useFilter('status.invalid');
  filters['invalid_hyp'] = useFilter('status.invalid_hyp');
  filters['considered_valid'] = useFilter('status.considered_valid');
  filters['untried'] = useFilter('status.untried');
  filters['dead'] = useFilter('status.dead');
  filters['inconsistent'] = useFilter('status.inconsistent');

  return {
    contextMenu: getContextMenu(EFilterType.STATUS, filters),
    show: (statusData: Property) =>
      filterStatus(statusData.status),
  };
}

export function useKindPropertiesFilter(): IFilterState {
  const filters: { [key: string]: setting } = {};

  filters['assert'] = useFilter('kind.assert');
  filters['invariant'] = useFilter('kind.invariant');
  filters['variant'] = useFilter('kind.variant');
  filters['requires'] = useFilter('kind.requires');
  filters['ensures'] = useFilter('kind.ensures');
  filters['instance'] = useFilter('kind.instance');
  filters['assigns'] = useFilter('kind.assigns');
  filters['froms'] = useFilter('kind.froms');
  filters['allocates'] = useFilter('kind.allocates');
  filters['behavior'] = useFilter('kind.behavior',);
  filters['reachable'] = useFilter('kind.reachable',);
  filters['axiomatic'] = useFilter('kind.axiomatic');
  filters['pragma'] = useFilter('kind.pragma');
  filters['assumes'] = useFilter('kind.assumes');
  filters['others'] = useFilter('kind.others');

  return {
    contextMenu: getContextMenu(EFilterType.KIND, filters),
    show: (statusData: Property) => filterKind(statusData.kind),
  };
}

export function useEvaPropertiesFilter(): IFilterState {
  const filters: { [key: string]: setting } = {};

  filters['priority_only'] = useFilter('eva.priority_only');
  filters['data_tainted_only'] = useFilter('eva.data_tainted_only');
  filters['ctrl_tainted_only'] = useFilter('eva.ctrl_tainted_only');

  return {
    contextMenu: getContextMenu(EFilterType.EVA, filters),
    show: (statusData: Property) => filterEva(statusData),
  };
}

// --------------------------------------------------------------------------
// --- Property Columns
// --------------------------------------------------------------------------

const renderCode: Renderer<string> =
  (text: string): JSX.Element =>
    (<Code className="code-column" title={text}>{text}</Code>);

const renderTag: Renderer<States.Tag> =
  (d: States.Tag): JSX.Element =>
    (<Label label={d.label ?? d.name} title={d.descr} />);

const renderNames: Renderer<string[]> =
  (names: string[]): JSX.Element | null => {
    const label = names?.join(': ');
    return (label ? <Label label={label} /> : null);
  };

const renderDir: Renderer<Ast.source> =
  (loc: Ast.source): JSX.Element =>
    (<Code className="code-column" label={loc.dir} title={loc.file} />);

const renderFile: Renderer<Ast.source> =
  (loc: Ast.source): JSX.Element =>
    (<Code className="code-column" label={loc.base} title={loc.file} />);

const renderPriority: Renderer<boolean> =
  (prio: boolean): JSX.Element | null =>
    (prio ? <Icon id="ATTENTION" /> : null);

export const renderTaint: Renderer<States.Tag> =
  (taint: States.Tag): JSX.Element | null => {
    let id = null;
    let color = 'black';
    const status = taint.name as taintStatus;
    switch (status) {
      case taintStatus.not_tainted:
        id = 'DROP.EMPTY'; color = '#00B900'; break;
      case taintStatus.direct_taint:
        id = 'DROP.FILLED'; color = '#882288'; break;
      case taintStatus.indirect_taint:
        id = 'DROP.FILLED'; color = '#73BBBB'; break;
      case taintStatus.error:
        id = 'HELP'; break;
      case taintStatus.not_applicable:
        id = 'MINUS'; break;
      case taintStatus.not_computed:
        break;
      default:
        throw status satisfies never;
    }
    return (id ? <Icon id={id} fill={color} title={taint.descr} /> : null);
  };

function ColumnCode<Row>(props: ColumnProps<Row, string>): JSX.Element {
  return <Column render={renderCode} {...props} />;
}

function ColumnTag<Row>(props: ColumnProps<Row, States.Tag>): JSX.Element {
  return <Column render={renderTag} {...props} />;
}

// --------------------------------------------------------------------------
// --- Properties Table
// -------------------------------------------------------------------------

const bySource =
  Compare.byFields<Ast.source>({ file: Compare.alpha, line: Compare.number });

const byStatus =
  Compare.byRank(
    'inconsistent',
    'invalid',
    'invalid_under_hyp',
    'unknown',
    'valid_under_hyp',
    'valid',
    'invalid_but_dead',
    'unknown_but_dead',
    'valid_but_dead',
    'never_tried',
    'considered_valid',
  );

const byTaint =
  Compare.option(
    Compare.byRank(
      'direct_taint',
      'indirect_taint',
      'not_tainted',
      'error',
      'not_applicable',
      'not_computed',
    )
  );

function declName(d: Ast.decl): string {
  return States.getDeclaration(d).name;
}
function markerSource(m: Ast.marker | undefined): Ast.source | undefined {
  return States.getMarker(m).sloc;
}

const byProperty: Compare.ByFields<Property> = {
  status: byStatus,
  scope: Compare.defined(Compare.lift(declName, Compare.alpha)),
  source: bySource,
  kind: Compare.structural,
  alarm: Compare.defined(Compare.alpha),
  names: Compare.array(Compare.alpha),
  predicate: Compare.defined(Compare.alpha),
  key: Compare.string,
  kinstr: Compare.lift(markerSource, (Compare.defined(bySource))),
  priority: Compare.structural,
  taint: byTaint,
};

const byDir = Compare.byFields<Ast.source>({ dir: Compare.alpha });
const byFile = Compare.byFields<Ast.source>({ base: Compare.alpha });

const byColumn: Arrays.ByColumns<Property> = {
  dir: Compare.byFields<Property>({ source: byDir }),
  file: Compare.byFields<Property>({ source: byFile }),
};

class PropertyModel extends Arrays.CompactModel<PropKey, Property> {

  private filterScope: States.Scope;

  constructor() {
    super((p: Property) => p.key);
    this.setOrderingByFields(byProperty);
    this.setColumnOrder(byColumn);
    this.setFilter(this.filterItem.bind(this));
  }

  setFilterScope(scope: States.Scope): void {
    this.filterScope = scope;
    if (filter('currentScope')) this.reload();
  }

  filterItem(prop: Property): boolean {
    const current = this.filterScope;
    const filtering = current && filter('currentScope');
    const filterScope = filtering ? prop.scope === current : true;
    return filterScope && filterSource(prop) && filterProperty(prop);
  }

}

// --------------------------------------------------------------------------
// --- Property Filter Form
// -------------------------------------------------------------------------

const Reload = new Dome.Event('ivette.properties.reload');

interface SectionProps {
  label: string;
  prefix?: TFilterType;
  unfold?: boolean;
  children: React.ReactNode;
}

export function onContextMenu(prefix: string): void {
  const items: Dome.PopupMenuItem[] = [
    {
      label: 'Reset to default',
      onClick: () => resetFilters(prefix),
    },
    {
      label: 'Select all',
      onClick: () => resetFilters(prefix, true),
    },
    {
      label: 'Deselect all',
      onClick: () => resetFilters(prefix, false),
    },
  ];
  Dome.popupMenu(items);
}

function FilterSection(props: SectionProps): JSX.Element {
  const settings = `properties-section-${props.label}`;
  const { label, prefix } = props;
  const filterButtonProps =
    prefix ? {
      icon: 'TUNINGS',
      title: `Configure filters`,
      onClick: () => onContextMenu(prefix),
    } : undefined;
  const update = Dome.useForceUpdate();
  Settings.useWindowSettingsEvent(update);
  const summary = prefix ? filterSummary(prefix) : undefined;
  return (
    <Section
      label={label}
      settings={settings}
      defaultUnfold={props.unfold}
      infos={summary}
      rightButtonProps={filterButtonProps}
    >
      {props.children}
    </Section>
  );
}

interface CheckFieldProps {
  label: string;
  title?: string;
  highlight?: boolean; // Highlights the label when the value is [highlight]
  path: string;
}

function CheckField(props: CheckFieldProps): JSX.Element {
  const [value, setValue] = useFilter(props.path);
  const onChange = (): void => { setValue(); Reload.emit(); };
  return (
    <Checkbox
      style={{
        color: (props.highlight === value) ? 'red' : '',
      }}
      label={props.label}
      title={props.title}
      value={value}
      onChange={onChange}
    />
  );
}

/* eslint-disable max-len */

function PropertyFilter(): JSX.Element {
  const namesState = useFilterStr("names");
  const checkerNames = (names: string | undefined): Form.FieldError => {
    if (names === undefined || names === "" || names.length > 1) return true;
    return "At least 2 characters";
  };

  const getCheckBox = (type: TFilterType): JSX.Element => {
    return <> {
      Object.entries(DEFAULTS)
        .filter(([key,]) => key.startsWith(type + "."))
        .map(([key, elt]) =>
          <CheckField key={key} label={elt.label} path={key} title={elt.title} />
        )
    }</>;
  };

  return (
    <Scroll>
      <CheckField label={DEFAULTS.currentScope.label} path="currentScope" />
      <Section
        label="Search"
        defaultUnfold={true}
        className="properties-section-names"
        infos={Form.isValid(namesState.error) && namesState.value.length >= 2 ? "Active" : ""}
        summary={!Form.isValid(namesState.error) ?
          <Icon id='WARNING' kind="warning" title={`Errors in section`} /> : undefined}
      >
        <Form.TextField
          label={""}
          placeholder="Names"
          title={'Filters names based on regular expressions; enter 2 or more characters to filter'}
          state={namesState as Form.FieldState<string | undefined>}
          checker={checkerNames}
        />
      </Section>

      <FilterSection label="Source" prefix="source"> {getCheckBox("source")} </FilterSection>
      <FilterSection label="Status" prefix="status" unfold> {getCheckBox("status")} </FilterSection>
      <FilterSection label="Property kind" prefix="kind"> {getCheckBox("kind")} </FilterSection>
      <FilterSection label="Alarms kind" prefix="alarms"> {getCheckBox("alarms")} </FilterSection>
      <FilterSection label="Eva"> {getCheckBox("eva")} </FilterSection>
    </Scroll>
  );
}

/* eslint-enable max-len */

// -------------------------------------------------------------------------
// --- Property Columns
// -------------------------------------------------------------------------

function PropertyColumns(): JSX.Element {
  const getDecl = States.useSyncArrayGetter(Ast.declAttributes);
  const statusDict = States.useTags(Properties.propStatusTags);
  const kindDict = States.useTags(Properties.propKindTags);
  const alarmDict = States.useTags(Properties.alarmsTags);
  const taintDict = States.useTags(taintStatusTags);

  const getScope = React.useCallback(
    ({ scope }: { scope: Property['scope'] }) => getDecl(scope)?.name
    , [getDecl]);
  const getStatus = React.useCallback(
    ({ status: st }: Property) => (statusDict.get(st) ?? { name: st })
    , [statusDict]);
  const getKind = React.useCallback(
    ({ kind: kd }: Property) => (kindDict.get(kd) ?? { name: kd })
    , [kindDict]);
  const getAlarm = React.useCallback(
    ({ alarm }: Property) => (
      alarm === undefined ? alarm : (alarmDict.get(alarm) ?? { name: alarm })
    ), [alarmDict]);
  const getTaint = React.useCallback(
    (p: Property) => (
      'taint' in p ? taintDict.get(p.taint) ?? { name: p.taint } : undefined
    ), [taintDict]);

  return (
    <>
      <Column
        id="dir"
        label="Directory"
        width={240}
        visible={false}
        getter={(prop: Property) => prop?.source}
        render={renderDir}
      />
      <Column
        id="file"
        label="File"
        width={120}
        getter={(prop: Property) => prop?.source}
        render={renderFile}
      />
      <ColumnCode id="scope" label="Scope" getter={getScope} width={120} />
      <ColumnTag id="kind" label="Property kind" getter={getKind} width={120} />
      <ColumnTag id="alarm" label="Alarms" getter={getAlarm} width={160} />
      <Column
        id="names"
        label="Names"
        width={240}
        visible={false}
        render={renderNames}
      />
      <ColumnCode id="predicate" label="Predicate" fill />
      <ColumnCode id="descr" label="Property" fill visible={false} />
      <Column
        id="priority"
        label="Priority"
        title="Properties invalid in some context of the Eva analysis"
        icon="ATTENTION"
        width={30}
        visible={false}
        align="center"
        getter={(prop: EvaAst.propertiesData) => prop?.priority}
        render={renderPriority}
      />
      <ColumnTag
        id="taint"
        label="Taint"
        title="Properties tainted according to the Eva taint domain"
        icon="PAINTBRUSH"
        width={30}
        visible={false}
        align="center"
        getter={getTaint}
        render={renderTaint}
      />
      <ColumnTag
        id="status"
        label="Status"
        width={100}
        align="center"
        getter={getStatus}
      />
    </>
  );
}

function FilterRatio({ model }: { model: PropertyModel }): JSX.Element {
  Models.useModel(model);
  const [filtered, total] = [model.getRowCount(), model.getTotalRowCount()];
  return (
    <Label
      className="component-info"
      title="Displayed Properties / Total"
      display={filtered !== total || true}
    >
      {filtered} / {total}
    </Label>
  );
}

// -------------------------------------------------------------------------
// --- Properties Table
// -------------------------------------------------------------------------

type PropsModel = States.ArrayProxy<PropKey, Properties.statusData>;
type EvapsModel = States.ArrayProxy<PropKey, EvaAst.propertiesData>;

function populateModel(
  model: PropertyModel,
  props: PropsModel,
  evaps: EvapsModel
): void {
  model.removeAllData();
  props.forEach((prop) => {
    const { key } = prop;
    const eva = evaps.getData(key);
    model.setData(key, { ...eva, ...prop });
  });
  model.reload();
}

export default function RenderProperties(): JSX.Element {

  // Hooks
  const model = React.useMemo(() => new PropertyModel(), []);
  const props = States.useSyncArrayProxy(Properties.status);
  const evaps = States.useSyncArrayProxy(EvaAst.properties);
  React.useEffect(() => {
    populateModel(model, props, evaps);
  }, [model, props, evaps]);

  const { marker, scope } = States.useCurrentLocation();

  const [showFilter, flipFilter] =
    Dome.useFlipSettings('ivette.properties.showFilter', true);

  // Updating the filter
  Dome.useEvent(Reload, model.reload);
  React.useEffect(() => {
    model.setFilterScope(scope);
  }, [model, scope]);

  // Callbacks

  const onPropertySelection = React.useCallback(
    (p: Property) => States.setSelected(p.key), []
  );

  return (
    <>
      <TitleBar help="framac-properties">
        <FilterRatio model={model} />
        <IconButton
          icon="CLIPBOARD"
          selected={showFilter}
          onClick={flipFilter}
          title="Toggle filters panel"
        />
      </TitleBar>
      <RSplit
        settings="ivette.properties.filterSplit"
        defaultPosition={200}
        unfold={showFilter}
      >
        <Table<string, Property>
          model={model}
          sorting={model}
          selection={marker}
          onSelection={onPropertySelection}
          settings="ivette.properties.table"
        >
          <PropertyColumns />
        </Table>
        <PropertyFilter />
      </RSplit>
    </>
  );
}

// --------------------------------------------------------------------------
