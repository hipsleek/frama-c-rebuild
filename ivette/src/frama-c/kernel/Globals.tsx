/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Frama-C Globals
// --------------------------------------------------------------------------

import React from 'react';
import InfiniteScroll from 'react-infinite-scroller';

import * as Dome from 'dome';
import { classes } from 'dome/misc/utils';
import { alpha } from 'dome/data/compare';
import { Section, Item, SidebarTitle, makeBadge } from 'dome/frame/sidebars';
import * as Buttons from 'dome/controls/buttons';
import { Label } from 'dome/controls/labels';
import * as Toolbar from 'dome/frame/toolbars';
import { Hbox } from 'dome/layout/boxes';
import * as Forms from 'dome/layout/forms';
import { Tree, Node } from 'dome/frame/tree';
import { RState } from 'dome/data/states';
import { Dropdown } from 'dome/dialogs';
import { Icon } from 'dome/controls/icons';
import * as Json from 'dome/data/json';
import { useWindowSettings } from 'dome/data/settings';

import * as Ivette from 'ivette';
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';
import * as Locations from 'frama-c/kernel/Locations';
import path from 'path';

// --------------------------------------------------------------------------
// --- Global Search Hints
// --------------------------------------------------------------------------

function globalHints(): Ivette.Hint[] {
  const globals = States.getSyncArray(Ast.declAttributes).getArray();
  return globals.map((g: Ast.declAttributesData) => ({
    id: g.decl,
    name: g.name,
    label: g.label,
    onClick: () => States.setCurrentScope(g.decl),
  }));
}

const globalMode: Ivette.SearchProps = {
  id: 'frama-c.kernel.globals',
  label: 'Globals',
  title: 'Lookup for Global Declarations',
  placeholder: 'declaration',
  hints: globalHints,
};

function resetMode(enabled: boolean): void {
  Ivette.updateSearchMode({ id: globalMode.id, enabled });
  Ivette.selectSearchMode(globalMode.id);
}

{
  Ivette.registerSearchMode(globalMode);
  Dome.find.on(() => Ivette.focusSearchMode(globalMode.id));
  Server.onReady(() => resetMode(true));
  Server.onShutdown(() => resetMode(false));
  resetMode(false);
}

// --------------------------------------------------------------------------
// --- Menu item
// --------------------------------------------------------------------------

export type setting = [boolean, () => void]
interface MenuItemProps {
  label: string,
  state: setting,
  title?: string,
  enabled?: boolean
}

export function menuItem(props: MenuItemProps): Buttons.ItemProps {
  const { label, state, title, enabled = true } = props;
  const [b, flip] = state;
  return {
    label: label,
    enabled: enabled,
    title: title || '',
    checked: b,
    onClick: flip,
  };
}

// --------------------------------------------------------------------------
// --- Lists
// --------------------------------------------------------------------------

interface ScrollableParent {
  scrollableParent: React.RefObject<HTMLDivElement>;
}

type InfiniteScrollListProps = {
  children: JSX.Element[];
} & ScrollableParent

type ListProps = {
  name: string;
  total: number;
  filteringMenuItems?: Dome.PopupMenuItem[];
  filteringMenu?: React.JSX.Element;
  children: JSX.Element[];
} & InfiniteScrollListProps


function InfiniteScrollList(props: InfiniteScrollListProps): JSX.Element {
  const [displayedCount, setDisplayedCount] = React.useState(100);
  const { children, scrollableParent } = props;
  const count = children.length;
  return (
    <InfiniteScroll
      pageStart={0}
      loadMore={() => setDisplayedCount(displayedCount + 100)}
      hasMore={displayedCount < count}
      loader={<Label key={-1}>Loading more...</Label>}
      useWindow={false}
      getScrollParent={() => scrollableParent.current}
    >
      {children.slice(0, displayedCount)}
    </InfiniteScroll>
  );

}

function List(props: ListProps): JSX.Element {
  const { name, total, filteringMenuItems, filteringMenu,
    children, scrollableParent } = props;
  const Name = name.charAt(0).toUpperCase() + name.slice(1);
  const count = children.length;

  const filterButtonProps = {
    icon: 'FILTER',
    title: `${Name}s filtering options (${count} / ${total})`,
    onClick: () => filteringMenuItems && Dome.popupMenu(filteringMenuItems),
  };

  let contents;

  if (count <= 0 && total > 0) {
    const button = <Buttons.Button {...filterButtonProps}
      label={`${Name}s filters`} />;
    contents =
      <div className='dome-xSideBarSection-content'>
        <label className='globals-info'>
          All {name}s are filtered. Try adjusting {name} filters.
        </label>
        {filteringMenu
          ? <Dropdown control={button}>{filteringMenu}</Dropdown>
          : button
        }
      </div>;
  }
  else if (total <= 0) {
    contents =
      <div className='dome-xSideBarSection-content'>
        <label className='globals-info'>
          There is no {name} to display.
        </label>
      </div>;
  }
  else {
    contents =
      <InfiniteScrollList scrollableParent={scrollableParent}>
        {children}
      </InfiniteScrollList>;
  }

  return (
    <Section
      label={`${Name}s`}
      title={`${Name}s ${count} / ${total}`}
      defaultUnfold
      settings={`frama-c.sidebar.${name}s`}
      rightButtonProps={filterButtonProps}
      filteringMenu={filteringMenu}
      summary={[count]}
      className='globals-section'
    >
      {contents}
    </Section>
  );
}


// --------------------------------------------------------------------------
// --- Function items
// --------------------------------------------------------------------------

interface FctItemProps {
  fct: Ast.functionsData,
  scope: States.Scope,
  addIcon: boolean
}

function FctItem(props: FctItemProps): JSX.Element {
  const { fct, scope, addIcon } = props;
  const { name, signature, main, stdlib, builtin, defined, decl } = fct;
  const { scopes, label: selectionLabel } = Locations.useSelection();
  const className = classes(
    main && 'globals-main',
    (stdlib || builtin) && 'globals-stdlib',
  );
  const attributes = classes(
    main && '(main)',
    !stdlib && !builtin && !defined && '(ext)',
  );
  return (
    <Item
      key={decl}
      icon={addIcon ? 'FUNCTION' : undefined}
      kind={'positive'}
      className={className}
      label={name}
      title={signature}
      selected={decl === scope}
      onSelection={() => States.setCurrentScope(decl)}
    >
      {attributes && <span className="globals-attr">{attributes}</span>}
      {scopes && scopes.includes(decl) &&
        <Icon id='MULTICHECK' kind='selected'
          title={'Selected in the Locations panel: ' + selectionLabel} />
      }
    </Item>
  );
}

// --------------------------------------------------------------------------
// --- Generic filter
// --------------------------------------------------------------------------

interface LocalFilter extends Ast.filter { value: string }

function isVisible(
  value: { filters: [string, boolean][] },
  localFilters: LocalFilter[]
): boolean {
  return localFilters.every(f => {
    const current = value.filters.find(([k,]) => k === f.id);
    /**
     * If f.value is ‘all’ or if the filter does not exist in
     * the current value: returns true.
     * Otherwise, the returned value depends on the match between
     * the current value and the selected filter.
     */
    return current === undefined || f.value === "all"
      || (f.value === f.positive_label && current[1])
      || (f.value === f.negative_label && !current[1]);
  });
}

type FilterKind = 'functions' | 'variables'
/**
 * This hook returns the list of Boolean filters to display and
 * a function to modify the value of a filter.
 */
function useFilterLocal(filters: Ast.filter[], kind: FilterKind)
  : [LocalFilter[], setFilterValue: (id: string, value: string) => void] {
  const name = `ivette.${kind}.filters`;
  const decode = Json.jDict(Json.jString);
  const [savedFilters, setSavedFilters] = useWindowSettings(name, decode, {});

  const setFilterValue = React.useCallback(
    (id: string, value: string): void => {
      const newObj = structuredClone(savedFilters);
      newObj[id] = value;
      setSavedFilters(newObj);
    }, [savedFilters, setSavedFilters]);

  function getValue(f: Ast.filter): string {
    if (f.positive_default && f.negative_default) return 'all';
    else if (f.positive_default) return f.positive_label;
    else if (f.negative_default) return f.negative_label;
    else return 'all';
  }

  const localFilters = React.useMemo(() => filters.map(f => {
    const value = savedFilters[`${f.id}`] ?? getValue(f);
    return { ...f, value };
  }), [filters, savedFilters]);

  return [localFilters, setFilterValue];
}

function useFiltersFlipSettings(label: string, type: string, b: boolean)
  : setting {
  return Dome.useFlipSettings(`ivette.${type}.${label}`, b);
}

function getSelectElement(descr: string, kind: string)
  : Buttons.SelectButtonElement {
  const label = descr.replace(kind, "").replace(/\s*\([^)]*\)/g, '');
  const title = `Show only ${descr}`;
  return { id: descr, label, title };
}

function getFilterButtonProps(f: LocalFilter, kind: string)
  : Buttons.SelectButtonElement[] {
  const allTitle = `Show both ${f.positive_label} and ${f.negative_label}`;
  return [
    { id: 'all', label: 'All', title: allTitle },
    getSelectElement(f.positive_label, kind),
    getSelectElement(f.negative_label, kind)
  ];
}

// --------------------------------------------------------------------------
// --- Functions Section
// --------------------------------------------------------------------------

interface FunctionFilterRet {
  contextFctFilter: React.JSX.Element,
  multipleSelection: States.Scope[],
  showFunction: (fct: Ast.functionsData) => boolean,
  isSelected: (fct: Ast.functionsData) => boolean
}

export function useFunctionFilter(): FunctionFilterRet {
  const { scopes } = Locations.useSelection();
  const multipleSelection = React.useMemo(() => scopes || [], [scopes]);
  const multipleSelectionActive = multipleSelection.length > 0;

  const isSelected = React.useMemo(() => {
    return (fct: Ast.functionsData): boolean => {
      const idx = multipleSelection.findIndex((s) => s === fct.decl);
      return 0 <= idx;
    };
  }, [multipleSelection]);

  const filters = States.useRequestStable(Ast.getFunctionsFilters, null);
  const [localFilters, setLocalFilters] = useFilterLocal(filters, 'functions');
  const selectedState = useFiltersFlipSettings('selected', 'functions', false);
  const [selected,] = selectedState;

  const showFunction = React.useMemo(() => {
    return (fct: Ast.functionsData): boolean => {
      const visible = isVisible(fct, localFilters);
      const local = !multipleSelectionActive || !selected || isSelected(fct);
      return visible && local;
    };
  }, [localFilters, selected, isSelected, multipleSelectionActive
  ]);

  const itemsComp = localFilters.map(e =>
    <Buttons.SelectButton
      key={e.id}
      buttonList={getFilterButtonProps(e, 'functions')}
      selected={e.value}
      onSelection={(a: string) => setLocalFilters(e.id, a)}
    />
  );

  itemsComp.push(<Toolbar.Button
    key='selectedOnly'
    label='Selected only'
    selected={selectedState[0]}
    onClick={selectedState[1]}
    title='Show only the functions selected in the Locations panel'
    disabled={!multipleSelectionActive} />
  );

  const contextFctFilter = <Buttons.Multiselect title="Show functions">
    {itemsComp}</Buttons.Multiselect>;

  return { contextFctFilter, multipleSelection, showFunction, isSelected };
}

export function Functions(props: ScrollableParent): JSX.Element {
  // Hooks
  const scope = States.useCurrentScope();

  const fcts = States.useSyncArrayData(Ast.functions);
  const { showFunction, contextFctFilter } = useFunctionFilter();

  // Filtered
  const items =
    fcts
      .filter(showFunction)
      .sort((f, g) => alpha(f.name, g.name))
      .map((fct) =>
        <FctItem key={fct.decl} fct={fct} scope={scope} addIcon={false} />);

  return (
    <List
      name="function"
      total={fcts.length}
      filteringMenu={contextFctFilter}
      scrollableParent={props.scrollableParent}
    >
      {items}
    </List>
  );
}

// --------------------------------------------------------------------------
// --- Global variables section
// --------------------------------------------------------------------------

interface VarItemProps {
  variable: Ast.globalsData,
  scope: States.Scope,
  addIcon: boolean
}

function VarItem(props: VarItemProps): JSX.Element {
  const { variable, scope, addIcon } = props;
  const { name, type, decl } = variable;
  const varMarker = React.useMemo(
    () => States.getDeclaration(decl).self, [decl]);
  const { markers, label: selectionLabel } = Locations.useSelection();
  return (
    <Item
      key={decl}
      icon={addIcon ? 'VARIABLE' : undefined}
      kind={'positive'}
      label={name}
      title={type}
      selected={decl === scope}
      onSelection={() => States.setCurrentScope(decl)}
    >
      {markers && markers.includes(varMarker) &&
        <Icon id='MULTICHECK' kind='selected'
          title={'Selected in the Locations panel: ' + selectionLabel} />
      }
    </Item>
  );
}

interface VariablesFilterRet {
  contextVarFilter: React.JSX.Element,
  showVariable: (vi: Ast.globalsData) => boolean,
}

export function useVariableFilter(): VariablesFilterRet {
  const filters = States.useRequestStable(Ast.getVariablesFilters, null);
  const [localFilters, setLocalFilters] = useFilterLocal(filters, 'variables');

  const showVariable = React.useMemo(() => {
    return (vi: Ast.globalsData): boolean => {
      const visible = isVisible(vi, localFilters);
      /* Never show global variables representing string literals.
         If needed, add a new filter to show these variables. */
      return !vi.stringLiteral && visible;
    };
  }, [localFilters]);

  const itemsComp = localFilters.map(e =>
    <Buttons.SelectButton
      key={e.id}
      buttonList={getFilterButtonProps(e, 'variables')}
      selected={e.value}
      onSelection={(a: string) => setLocalFilters(e.id, a)}
    />
  );
  const contextVarFilter = <Buttons.Multiselect title='Show variables'>
    {itemsComp}
  </Buttons.Multiselect>;

  return { contextVarFilter, showVariable };
}

export function Variables(props: ScrollableParent): JSX.Element {
  // Hooks
  const scope = States.useCurrentScope();
  const variables = States.useSyncArrayData(Ast.globals);
  const { showVariable, contextVarFilter } = useVariableFilter();

  // Filtered
  const items =
    variables
      .filter(showVariable)
      .sort((v1, v2) => alpha(v1.name, v2.name))
      .map((v) =>
        <VarItem key={v.decl} scope={scope} variable={v} addIcon={false} />);

  return (
    <List
      name="variable"
      total={variables.length}
      filteringMenu={contextVarFilter}
      scrollableParent={props.scrollableParent}
    >
      {items}
    </List>
  );
}


// --------------------------------------------------------------------------
// --- Generic Declaration Section
// --------------------------------------------------------------------------

interface DeclarationsProps {
  id: string;
  label: string;
  title: string;
  filter: (props: Ast.declAttributesData) => boolean;
  defaultUnfold?: boolean;
}

const isAcsl = (d: Ast.declAttributesData): boolean => {
  switch (d.kind) {
    case 'TYPEDEF':
    case 'ENUM':
    case 'UNION':
    case 'STRUCT':
    case 'GLOBAL':
    case 'FUNCTION':
      return false;
    default:
      return true;
  }
};

const getIcon = (d: Ast.declAttributesData): string => {
  switch (d.kind) {
    case 'TYPEDEF':
    case 'ENUM':
    case 'UNION':
    case 'STRUCT':
      return 'TYPE';
    default:
      return d.kind;
  }
};

const getName = (d: Ast.declAttributesData): string => {
  switch (d.kind) {
    case 'TYPEDEF': return 'typedef ' + d.name;
    case 'ENUM': return 'enum ' + d.name;
    case 'UNION': return 'union ' + d.name;
    case 'STRUCT': return 'struct ' + d.name;
    default: return d.name;
  }
};

interface AttrItemProps {
  dattrs: Ast.declAttributesData,
  scope: States.Scope,
  addIcon: boolean
}

function AttrItem(props: AttrItemProps): JSX.Element {
  const { dattrs, scope, addIcon } = props;
  const { decl, label } = dattrs;
  const name = getName(dattrs);
  const icon = !addIcon ? undefined : getIcon(dattrs);
  const iconkind = isAcsl(dattrs) ? 'default' : 'positive';
  return (
    <Item
      key={decl}
      label={name}
      icon={icon}
      kind={iconkind}
      title={label}
      selected={decl === scope}
      onSelection={() => States.setCurrentScope(decl)}
    />
  );
}

export function Declarations(props: DeclarationsProps): JSX.Element {
  const { id, label, title, filter, defaultUnfold = false } = props;
  const settings = React.useMemo(() => `frama-c.sidebar.${id}`, [id]);
  const data = States.useSyncArrayData(Ast.declAttributes);
  const scope = States.useCurrentScope();
  const items = React.useMemo(
    () =>
      data
        .filter(filter)
        .sort((d1, d2) => alpha(d1.name, d2.name))
        .map((d) =>
          <AttrItem key={d.decl} dattrs={d} scope={scope} addIcon={false} />)
    , [scope, data, filter]
  );
  return (
    <Section
      label={label}
      title={title}
      defaultUnfold={defaultUnfold}
      settings={settings}
      summary={[items.length]}
      className='globals-section'
    >
      {items}
    </Section>
  );
}

// --------------------------------------------------------------------------
// --- Types Section
// --------------------------------------------------------------------------

const filterTypes = (d: Ast.declAttributesData): boolean => {
  switch (d.kind) {
    case 'TYPEDEF':
    case 'ENUM':
    case 'UNION':
    case 'STRUCT':
      return true;
    default:
      return false;
  }
};

export function Types(): JSX.Element {
  return (
    <Declarations
      id='types'
      label='Types'
      title='Typedefs, Structs, Unions and Enums'
      filter={filterTypes}
    />
  );
}

// --------------------------------------------------------------------------
// --- Global Annotations Section
// --------------------------------------------------------------------------

interface AnnotFilterRet {
  contextAnnotFilter: React.JSX.Element,
  showAnnotation: (decl: Ast.declAttributesData) => boolean
}

export function useAnnotFilter(): AnnotFilterRet {
  const useFlipSettings = (label: string, b: boolean): setting => {
    return Dome.useFlipSettings('ivette.annotations.' + label, b);
  };

  const ltypesState = useFlipSettings('ltypes', false);
  const lfunPredsState = useFlipSettings('lfunPreds', false);
  const laxiomaticsState = useFlipSettings('laxiomatics', true);
  const lemmasState = useFlipSettings('lemmas', true);
  const lmodulesState = useFlipSettings('lmodules', true);
  const linvariantsState = useFlipSettings('linvariants', true);
  const lmodelState = useFlipSettings('lmodel', true);
  const lvolatileState = useFlipSettings('lvolatile', true);
  const slextensionsState = useFlipSettings('lextensions', false);

  const [ltypes,] = ltypesState;
  const [lfunPreds,] = lfunPredsState;
  const [laxiomatics,] = laxiomaticsState;
  const [lemmas,] = lemmasState;
  const [lmodules,] = lmodulesState;
  const [linvariants,] = linvariantsState;
  const [lmodel,] = lmodelState;
  const [lvolatile,] = lvolatileState;
  const [lextensions,] = slextensionsState;

  const showAnnotation = React.useMemo(() => {
    return (d: Ast.declAttributesData): boolean => {
      const annot = [
        'LTYPE', 'LFUNPRED', 'AXIOMATIC', 'LEMMA',
        'MODULE', 'INVARIANT', 'MODEL', 'VOLATILE', 'EXTENSION'
      ];
      const visible = annot.includes(d.kind)
        && (ltypes || d.kind !== 'LTYPE')
        && (lfunPreds || d.kind !== 'LFUNPRED')
        && (laxiomatics || d.kind !== 'AXIOMATIC')
        && (lemmas || d.kind !== 'LEMMA')
        && (lmodules || d.kind !== 'MODULE')
        && (linvariants || d.kind !== 'INVARIANT')
        && (lmodel || d.kind !== 'MODEL')
        && (lvolatile || d.kind !== 'VOLATILE')
        && (lextensions || d.kind !== 'EXTENSION');
      return visible;
    };
  }, [ltypes, lfunPreds, laxiomatics, lemmas, lmodules,
    linvariants, lmodel, lvolatile, lextensions
  ]);

  const contextMenuItems: Buttons.MultiselectItemProps[] = [
    menuItem({
      label: 'Show Logic Types',
      state: ltypesState
    }),
    menuItem({
      label: 'Show Predicates and Logic Functions',
      state: lfunPredsState
    }),
    menuItem({
      label: 'Show Axiomatic Definitions',
      state: laxiomaticsState
    }),
    menuItem({
      label: 'Show Lemmas',
      state: lemmasState
    }),
    menuItem({
      label: 'Show Logic Modules',
      state: lmodulesState
    }),
    menuItem({
      label: 'Show Invariants',
      state: linvariantsState
    }),
    menuItem({
      label: 'Show Model Fields',
      state: lmodelState
    }),
    menuItem({
      label: 'Show Volatile Annotations',
      state: lvolatileState
    }),
    menuItem({
      label: 'Show ACSL Extensions',
      state: slextensionsState
    }),
  ];

  const itemsComp = contextMenuItems.map(
    (e, i) => <Buttons.MultiselectItem key={i} item={e} />);
  const contextAnnotFilter =
    <Buttons.Multiselect>{itemsComp}</Buttons.Multiselect>;

  return {
    contextAnnotFilter,
    showAnnotation: showAnnotation,
  };
}

export function GlobalAnnots(): JSX.Element {
  return <>
    <Declarations
      id='ltypes'
      label='Logic Types'
      title='Logic Types'
      filter={(d): boolean => d.kind === 'LTYPE'}
    />
    <Declarations
      id='lfun-preds'
      label='Predicates and Logic Functions'
      title='Predicates and Logic Functions'
      filter={(d): boolean => d.kind === 'LFUNPRED'}
    />
    <Declarations
      id='laxiomatics'
      label='Axiomatic Definitions'
      title='Axiomatic Definitions'
      filter={(d): boolean => d.kind === 'AXIOMATIC'}
    />
    <Declarations
      id='lemmas'
      label='Lemmas'
      title='Lemmas'
      filter={(d): boolean => d.kind === 'LEMMA'}
    />
    <Declarations
      id='lmodules'
      label='Logic Modules'
      title='Logic Modules'
      filter={(d): boolean => d.kind === 'MODULE'}
    />
    <Declarations
      id='linvariants'
      label='Invariants'
      title='Invariants'
      filter={(d): boolean => d.kind === 'INVARIANT'}
    />
    <Declarations
      id='lmodel'
      label='Model Fields'
      title='Model Fields'
      filter={(d): boolean => d.kind === 'MODEL'}
    />
    <Declarations
      id='lvolatile'
      label='Volatile Annotations'
      title='Volatile Annotations'
      filter={(d): boolean => d.kind === 'VOLATILE'}
    />
    <Declarations
      id='lextensions'
      label='ACSL Extensions'
      title='ACSL Extensions'
      filter={(d): boolean => d.kind === 'EXTENSION'}
    />
  </>;
}

// --------------------------------------------------------------------------
// --- Files Section
// --------------------------------------------------------------------------

interface Dir {
  id: string;
  label: string;
  path: string[],
  files: File[];
  dir: Dir[];
}
interface File {
  id: string;
  label: string,
  ext: string,
  path: string[],
  types: Ast.declAttributesData[],
  fcts: Ast.functionsData[],
  vars: Ast.globalsData[],
  annot: Ast.declAttributesData[]
}
type FileList = { [key: string]: File };


function Nodes(props: {
  dir: Dir[], files: File[],
  scope: States.Scope,
  prevCompact?: boolean
}): React.ReactNode {
  const { dir, files, prevCompact = false, scope } = props;
  const dirComp = dir.map(item =>
    <DirNode key={item.id} dir={item}
      scope={scope} prevCompact={prevCompact} />
  );
  const filesComp = files.map(file =>
    <Node icon={'FILE'} key={file.id} id={file.id}
      label={file.label} title={`${file.path.join('/')}/${file.label}`}
    ><Items {...file} scope={scope} /></Node>);

  return <>{dirComp}{filesComp}</>;
}

function DirNode(props: {
  dir: Dir,
  scope: States.Scope,
  prevCompact?: boolean
}): React.ReactNode {
  const { dir, scope, prevCompact = false } = props;
  const label = prevCompact ? `../${dir.label}` : dir.label;
  const toCompact = dir.dir.length === 1 && dir.files.length === 0;
  const path = `${dir.path.join('/')}/${dir.label}`;

  return toCompact ?
    <Nodes key={dir.id} dir={dir.dir} files={dir.files}
      scope={scope} prevCompact={true} />
    :
    <Node key={dir.id} icon={'FOLDER'} id={dir.id} label={label} title={path}>
      {dir.dir.length > 0 || dir.files.length > 0 ?
        <Nodes dir={dir.dir} files={dir.files}
          scope={scope} prevCompact={false} />
        : null}
    </Node>;
}

interface ItemsProps {
  types?: Ast.declAttributesData[],
  fcts?: Ast.functionsData[],
  vars?: Ast.globalsData[],
  annot?: Ast.declAttributesData[]
  init?: number; // default to 400 elements
  step?: number; // default to 400 elements
  addIcon?: boolean;
  scope: States.Scope;
}

function Items(props: ItemsProps): JSX.Element | null {
  const { types, fcts, vars, annot,
    init = 400, step = 400, addIcon = true, scope } = props;

  const [maxType, setMaxType] = React.useState(init);
  const [maxFct, setMaxFct] = React.useState(init);
  const [maxVar, setMaxVar] = React.useState(init);
  const [maxAnnot, setMaxAnnot] = React.useState(init);

  // Types
  const typeList = React.useMemo(() => {
    if (!types || types.length === 0) return null;
    return types.map(t =>
      <AttrItem key={t.decl} dattrs={t} scope={scope} addIcon={addIcon} />);
  }, [types, scope, addIcon]);
  const visibleTypes = React.useMemo(
    () => typeList?.slice(0, maxType) ?? [], [typeList, maxType]);
  const hasMoretypes = typeList ? typeList.length > maxType : false;

  // Functions
  const fctList = React.useMemo(() => {
    if (!fcts || fcts.length === 0) return null;
    return fcts.map(f =>
      <FctItem key={f.decl} fct={f} scope={scope} addIcon={addIcon} />);
  }, [fcts, scope, addIcon]);
  const visibleFcts = React.useMemo(
    () => fctList?.slice(0, maxFct) ?? [], [fctList, maxFct]);
  const hasMoreFcts = fctList ? fctList.length > maxFct : false;

  // Variables
  const varList = React.useMemo(() => {
    if (!vars || vars.length === 0) return null;
    return vars.map(v =>
      <VarItem key={v.decl} variable={v} scope={scope} addIcon={addIcon} />);
  }, [vars, scope, addIcon]);
  const visibleVars = React.useMemo(
    () => varList?.slice(0, maxVar) ?? [], [varList, maxVar]);
  const hasMoreVars = varList ? varList.length > maxVar : false;

  // Annotations
  const annotList = React.useMemo(() => {
    if (!annot || annot.length === 0) return null;
    return annot.map(a =>
      <AttrItem key={a.decl} dattrs={a} scope={scope} addIcon={addIcon} />);
  }, [annot, scope, addIcon]);
  const visibleAnnots = React.useMemo(
    () => annotList?.slice(0, maxAnnot) ?? [], [annotList, maxAnnot]);
  const hasMoreAnnot = annotList ? annotList.length > maxAnnot : false;

  const getButton = (
    set: React.Dispatch<React.SetStateAction<number>>,
    type?: string
  ): React.JSX.Element => (
    <Item
      label={`... show more ${type}`}
      onSelection={() => set(e => e + step)}
    />);

  return <>
    {visibleTypes}
    {hasMoretypes && getButton(setMaxType, 'types')}
    {visibleFcts}
    {hasMoreFcts && getButton(setMaxFct, 'functions')}
    {visibleVars}
    {hasMoreVars && getButton(setMaxVar, 'variables')}
    {visibleAnnots}
    {hasMoreAnnot && getButton(setMaxAnnot, 'annotations')}
  </>;
}


type FilesProps = {
  searchByName: string | undefined;
  unfoldAllState: RState<boolean | undefined>;
  showTypesState: [boolean, () => void];
  showFunction: (fct: Ast.functionsData) => boolean;
  showFctsState: [boolean, () => void];
  showVariable: (vi: Ast.globalsData) => boolean;
  showVarsState: [boolean, () => void];
  showAnnotation: (annot: Ast.declAttributesData) => boolean;
  showAnnotState: [boolean, () => void];
  dispInList: boolean;
  contextFctFilter: React.JSX.Element;
  contextVarFilter: React.JSX.Element;
  contextAnnotFilter: React.JSX.Element;
} & ScrollableParent

export function Files(props: FilesProps): JSX.Element {
  const { showTypesState, showFunction, showFctsState,
    showVariable, showVarsState, showAnnotation, showAnnotState,
    scrollableParent, dispInList, searchByName, unfoldAllState,
    contextFctFilter, contextAnnotFilter, contextVarFilter,
  } = props;
  const filterByName = React.useCallback((val: { name: string }): boolean => {
    return searchByName ? RegExp(searchByName, 'i').test(val.name) : true;
  }, [searchByName]);

  // Hooks
  const scope = States.useCurrentScope();

  // functions
  const fcts = States.useSyncArrayData(Ast.functions);
  const fctsSorted = React.useMemo(() =>
    fcts.sort((f, g) => alpha(f.name, g.name)
    ), [fcts]);
  const [showFcts,] = showFctsState;
  const _fctsFiltered = React.useMemo(() => {
    if (!showFcts) return [];
    return fctsSorted.filter(showFunction);
  }, [fctsSorted, showFunction, showFcts]);
  const fctsFiltered = React.useMemo(() => {
    if (!searchByName) return _fctsFiltered;
    return _fctsFiltered.filter(filterByName);
  }, [_fctsFiltered, searchByName, filterByName]);

  // Variables
  const variables = States.useSyncArrayData(Ast.globals);
  const varsSorted = React.useMemo(() =>
    variables.sort((f, g) => alpha(f.name, g.name)
    ), [variables]);
  const [showVars,] = showVarsState;
  const _varsFiltered = React.useMemo(() => {
    if (!showVars) return [];
    return varsSorted.filter(showVariable)
      .sort((v1, v2) => alpha(v1.name, v2.name));
  }, [varsSorted, showVariable, showVars]);
  const varsFiltered = React.useMemo(() => {
    if (!searchByName) return _varsFiltered;
    return _varsFiltered.filter(filterByName);
  }, [_varsFiltered, searchByName, filterByName]);

  // declaration for types and annotations
  const declarations = States.useSyncArrayData(Ast.declAttributes);
  const declarationsSorted = React.useMemo(() =>
    declarations.sort((v1, v2) => {
      const cmp = alpha(v1.kind, v2.kind);
      if (cmp !== 0) return cmp;
      return alpha(v1.name, v2.name);
    }), [declarations]);

  // types
  const [showTypes,] = showTypesState;
  const _typesFiltered = React.useMemo(() => {
    if (!showTypes) return [];
    return declarationsSorted.filter(filterTypes);
  }, [declarationsSorted, showTypes]);
  const typesFiltered = React.useMemo(() => {
    if (!searchByName) return _typesFiltered;
    return _typesFiltered.filter(filterByName);
  }, [_typesFiltered, searchByName, filterByName]);

  // Annotations
  const [showAnnot,] = showAnnotState;
  const _annotsFiltered = React.useMemo(() => {
    if (!showAnnot) return [];
    return declarationsSorted.filter(showAnnotation);
  }, [declarationsSorted, showAnnotation, showAnnot]);
  const annotsFiltered = React.useMemo(() => {
    if (!searchByName) return _annotsFiltered;
    return _annotsFiltered.filter(filterByName);
  }, [_annotsFiltered, searchByName, filterByName]);

  const files = React.useMemo(() => {
    const newFiles: FileList = {};
    function createFileIfNeeded(loc: Ast.source): void {
      if (!newFiles[loc.file])
        newFiles[loc.file] = {
          id: loc.file,
          label: loc.base,
          path: loc.dir.split('/').filter(Boolean),
          ext: path.extname(loc.base),
          types: [],
          fcts: [],
          vars: [],
          annot: []
        };
    }
    typesFiltered.forEach((elt) => {
      createFileIfNeeded(elt.source);
      newFiles[elt.source.file].types.push(elt);
    });
    fctsFiltered.forEach((fct) => {
      createFileIfNeeded(fct.sloc);
      newFiles[fct.sloc.file].fcts.push(fct);
    });
    varsFiltered.forEach((elt) => {
      createFileIfNeeded(elt.sloc);
      newFiles[elt.sloc.file].vars.push(elt);
    });
    annotsFiltered.forEach((elt) => {
      createFileIfNeeded(elt.source);
      newFiles[elt.source.file].annot.push(elt);
    });

    return Object.entries(newFiles).sort((f, g) => alpha(f[0], g[0]));
  }, [typesFiltered, fctsFiltered, varsFiltered, annotsFiltered]);

  const tree = React.useMemo(() => {
    const newTree: Dir[] = [];

    function addPath(current: Dir[], path: string[], index: number = 0): Dir {
      const dirpath = index === 0 ? [] : path.slice(0, index);
      const dirId = `${dirpath.join('/')}/${path[index]}`;

      const next = current.find(e => e.id === dirId) ??
        current[current.push(
          { id: dirId, label: path[index], path: dirpath, files: [], dir: [] }
        ) - 1];

      if (path.length === index + 1) return next;
      return addPath(next.dir, path, index + 1);
    }

    files.forEach(([, file]) => {
      addPath(newTree, file.path, 0).files.push(file);
    });

    return newTree;
  }, [files]);

  const [unfoldAll, setUnfoldAll] = unfoldAllState;
  return (
    <Tree
      unfoldAll={unfoldAll}
      setUnfoldAll={setUnfoldAll}
      sticky={true}
      className='sidebar-files-tree'
    >
      <div style={dispInList ? { display: 'none' } : { display: 'block' }}>
        <InfiniteScrollList scrollableParent={scrollableParent} >
          {Object.entries(tree).map(elt =>
            <DirNode key={elt[0]} dir={elt[1]} scope={scope} />
          )
          }
        </InfiniteScrollList >
      </div>
      <div style={dispInList ? { display: 'block' } : { display: 'none' }}>
        <InfiniteScrollList scrollableParent={scrollableParent} >
          {showTypes ? <Node key='typesFiltered' id='typesFiltered'
            label={'Types'} title={'Types'}
            actions={makeBadge(typesFiltered.length)}
          ><Items types={typesFiltered} scope={scope} addIcon={false} />
          </Node> : <></>
          }
          {showFcts ? <Node key='fctsFiltered' id='fctsFiltered'
            label={'Functions'} title={'Functions'} actions={<>
              <Dropdown control={<Buttons.IconButton icon='FILTER' />}
              >{contextFctFilter}</Dropdown>
              {makeBadge(fctsFiltered.length)}
            </>}
          ><Items fcts={fctsFiltered} scope={scope} addIcon={false} />
          </Node> : <></>
          }
          {showVars ? <Node key='varsFiltered' id='varsFiltered'
            label={'Variables'} title={'Variables'}
            actions={<>
              <Dropdown control={<Buttons.IconButton icon='FILTER' />}
              >{contextVarFilter}</Dropdown>
              {makeBadge(varsFiltered.length)}
            </>}
          ><Items vars={varsFiltered} scope={scope} addIcon={false} />
          </Node> : <></>
          }
          {showAnnot ? <Node key='annotsFiltered' id='annotsFiltered'
            label={'Annotations'} title={'Annotations'}
            actions={<>
              <Dropdown control={<Buttons.IconButton icon='FILTER' />}
              >{contextAnnotFilter}</Dropdown>
              {makeBadge(annotsFiltered.length)}
            </>}
          ><Items annot={annotsFiltered} scope={scope} addIcon={false} />
          </Node> : <></>
          }
        </InfiniteScrollList >
      </div>
    </Tree>
  );
}

interface SidebarFilesToolProps {
  searchByNameState: Forms.FieldState<string | undefined>;
  unfoldAllState: RState<boolean | undefined>;
  showTypesState: [boolean, () => void];
  showFctsState: [boolean, () => void];
  contextFctFilter: React.JSX.Element;
  showVarsState: [boolean, () => void];
  contextVarFilter: React.JSX.Element;
  showAnnotState: [boolean, () => void];
  contextAnnotFilter: React.JSX.Element;
}

function SidebarFilesTools(props: SidebarFilesToolProps): React.JSX.Element {
  const searchByNameState = props.searchByNameState;
  const [showTypes, flipShowTypes] = props.showTypesState;
  const [showFcts, flipShowFcts] = props.showFctsState;
  const [showVars, flipShowVars] = props.showVarsState;
  const [showAnnot, flipShowAnnot] = props.showAnnotState;

  const typesButton = <Toolbar.Button icon="T" title={'Show types'}
    selected={showTypes} onClick={() => flipShowTypes()} />;
  const fctsButton = <Toolbar.Button icon="F" title={'Show functions'}
    selected={showFcts} onClick={() => flipShowFcts()} />;
  const varsButton = <Toolbar.Button icon="V" title={'Show variables'}
    selected={showVars} onClick={() => flipShowVars()} />;
  const annotsButton = <Toolbar.Button icon="A" title={'Show annotations'}
    selected={showAnnot} onClick={() => flipShowAnnot()} />;

  return (
    <div className='sidebar-files-tools'>
      <Hbox className='sidebar-files-tools-filter'>
        <Hbox>
          {typesButton}
          <Toolbar.ButtonGroup>
            {fctsButton}
            <Dropdown control={<Toolbar.Button icon='FILTER' />}
            >{props.contextFctFilter}</Dropdown>
          </Toolbar.ButtonGroup>
          <Toolbar.ButtonGroup>
            {varsButton}
            <Dropdown control={<Toolbar.Button icon='FILTER' />}
            >{props.contextVarFilter}</Dropdown>
          </Toolbar.ButtonGroup>
          <Toolbar.ButtonGroup>
            {annotsButton}
            <Dropdown control={<Toolbar.Button icon='FILTER' />}
            >{props.contextAnnotFilter}</Dropdown>
          </Toolbar.ButtonGroup>
        </Hbox>
      </Hbox>
      <Forms.TextField
        label=''
        placeholder='Search'
        state={searchByNameState}
        actions={<Buttons.IconButton
          icon='TRASH'
          onClick={() =>
            searchByNameState.onChanged(undefined, undefined, false)}
        />}
      />
    </div>
  );
}

// --------------------------------------------------------------------------
// --- All globals
// --------------------------------------------------------------------------

export function GlobalByFiles(): JSX.Element {
  const scrollableArea = React.useRef<HTMLDivElement>(null);

  // display in list
  const [dispInList, flipDispInList] =
    Dome.useFlipSettings('ivette.sidebar.file.disp.inlist', false);

  // For input text
  const searchByNameState = Forms.useState<string | undefined>(undefined);

  // For unfoldAll tree
  const unfoldAllState = React.useState<boolean | undefined>(true);

  // Types
  const showTypesState =
    Dome.useFlipSettings('ivette.files.show.types', true);

  // Functions
  const { showFunction, contextFctFilter } = useFunctionFilter();
  const showFctsState =
    Dome.useFlipSettings('ivette.files.show.functions', true);

  // Variables
  const { showVariable, contextVarFilter } = useVariableFilter();
  const showVarsState =
    Dome.useFlipSettings('ivette.files.show.globals', true);

  // Annotations
  const { showAnnotation, contextAnnotFilter } = useAnnotFilter();
  const showAnnotState =
    Dome.useFlipSettings('ivette.files.show.annotations', true);

  const [unfoldAll, setUnfoldAll] = unfoldAllState;

  return (<>
    <SidebarTitle label='Files'>
      <Hbox>
        <Toolbar.ButtonGroup>
          <Toolbar.Button
            icon="CHEVRON.CONTRACT"
            title={'Fold all'}
            disabled={unfoldAll === false}
            onClick={() => setUnfoldAll(false)}
          />
          <Toolbar.Button
            icon='CHEVRON.EXPAND'
            title={'Unfold all'}
            disabled={unfoldAll}
            onClick={() => setUnfoldAll(true)}
          />
        </Toolbar.ButtonGroup>
        <Toolbar.ButtonGroup>
          <Toolbar.Button
            icon="ITEMS.LIST"
            title={'Display in list'}
            selected={dispInList}
            onClick={() => flipDispInList()}
          />
          <Toolbar.Button
            icon='TREE'
            title={'Display in tree'}
            selected={!dispInList}
            onClick={() => flipDispInList()}
          />
        </Toolbar.ButtonGroup>
      </Hbox>
    </SidebarTitle>
    <SidebarFilesTools
      searchByNameState={searchByNameState}
      unfoldAllState={unfoldAllState}
      showTypesState={showTypesState}
      showFctsState={showFctsState}
      contextFctFilter={contextFctFilter}
      showVarsState={showVarsState}
      contextVarFilter={contextVarFilter}
      showAnnotState={showAnnotState}
      contextAnnotFilter={contextAnnotFilter}
    />
    <div ref={scrollableArea} className="globals-scrollable-area">
      <Files scrollableParent={scrollableArea}
        searchByName={searchByNameState.value}
        unfoldAllState={unfoldAllState}
        showTypesState={showTypesState}
        showFctsState={showFctsState}
        showFunction={showFunction}
        showVarsState={showVarsState}
        showVariable={showVariable}
        showAnnotState={showAnnotState}
        showAnnotation={showAnnotation}
        dispInList={dispInList}
        contextFctFilter={contextFctFilter}
        contextVarFilter={contextVarFilter}
        contextAnnotFilter={contextAnnotFilter}
      />
    </div>
  </>
  );
}

export function GlobalDeclarations(): JSX.Element {
  const scrollableArea = React.useRef<HTMLDivElement>(null);
  return (<>
    <SidebarTitle label='Global Declarations' />
    <div ref={scrollableArea} className="globals-scrollable-area">
      <Types />
      <Variables scrollableParent={scrollableArea} />
      <Functions scrollableParent={scrollableArea} />
      <GlobalAnnots />
    </div>
  </>
  );
}
