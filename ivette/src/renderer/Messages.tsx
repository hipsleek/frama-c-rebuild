/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import * as React from 'react';
import * as Dome from 'dome';
import { IconButton } from 'dome/controls/buttons';
import { Label, Cell } from 'dome/controls/labels';
import { Page } from 'dome/text/pages';
import { Icon } from 'dome/controls/icons';
import { Scroll, Vbox } from 'dome/layout/boxes';
import { RSplit, BSplit } from 'dome/layout/splitters';
import * as Forms from 'dome/layout/forms';
import * as Arrays from 'dome/table/arrays';
import { Table, Column, Renderer } from 'dome/table/views';
import * as Compare from 'dome/data/compare';
import { State, GlobalState, useGlobalState } from 'dome/data/states';
import { Section } from 'dome/frame/sidebars';

import { TitleBar } from 'ivette';
import * as Display from 'ivette/display';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';
import * as Kernel from 'frama-c/kernel/api/services';
import * as Params from 'frama-c/kernel/api/parameters';
import * as Server from 'frama-c/server';

type Message = Kernel.messageData;
type logkind = Kernel.logkind;

// --------------------------------------------------------------------------
// --- Filters
// --------------------------------------------------------------------------

interface Search {
  category?: string;
  message?: string;
}

type KindFilter = Record<logkind, boolean>;
type PluginFilter = { [key: string]: boolean };
type EmitterFilter = {
  kernel: boolean;
  plugins: PluginFilter;
  others: boolean; // default for Frama-C plugins not in the plugins field.
};

interface Filter {
  currentDecl: boolean;
  search: Search;
  kind: KindFilter;
  emitter: EmitterFilter;
}

/* Only warnings and errors are shown by default. */
const kindFilter: KindFilter = {
  RESULT: false,
  FEEDBACK: false,
  DEBUG: false,
  WARNING: true,
  ERROR: true,
  FAILURE: true,
};

const emitterFilter = {
  kernel: true,
  plugins: {},
  others: true,
};

const defaultFilter: Filter = {
  currentDecl: false,
  search: {},
  kind: kindFilter,
  emitter: emitterFilter,
};

function filterKind(filter: KindFilter, msg: Message): boolean {
  return filter[msg.kind];
}

function filterEmitter(filter: EmitterFilter, msg: Message): boolean {
  if (msg.plugin === 'kernel')
    return filter.kernel;
  if (msg.plugin in filter.plugins)
    return filter.plugins[msg.plugin];
  return filter.others;
}

function searchCategory(
  search: string | undefined,
  msg: string | undefined
): boolean {
  if (!search || search.length < 2)
    return true;
  if (!msg)
    return false;
  const message = msg.toLowerCase();
  const array = search.toLowerCase().split(' ');
  let empty = true;
  let show = false;
  let hide = false;
  array.forEach((str: string) => {
    if (str.length > 1) {
      if (str.charAt(0) === '-') {
        if (message.includes(str.substring(1)))
          hide = true;
      }
      else if (message.includes(str))
        show = true;
      else
        empty = false;
    }
  });
  return (empty || show) && !hide;
}

function searchString(search: string | undefined, msg: string): boolean {
  if (!search || search.length < 3)
    return true;
  if (search.charAt(0) === '"' && search.slice(-1) === '"') {
    const exact = search.slice(1, -1);
    return msg.includes(exact);
  }
  const message = msg.toLowerCase();
  const array = search.toLowerCase().split(' ');
  let show = true;
  array.forEach((str: string) => {
    if (str.length > 1 && !message.includes(str))
      show = false;
  });
  return show;
}

function filterSearched(search: Search, msg: Message): boolean {
  return (searchString(search.message, msg.message) &&
    searchCategory(search.category, msg.category));
}

function filterDecl(
  filter: Filter,
  decl: Ast.decl | undefined,
  msg: Message
): boolean {
  if (filter.currentDecl)
    return (decl === msg.decl);
  return true;
}

function filterMessage(
  filter: Filter,
  decl: Ast.decl | undefined,
  msg: Message
): boolean {
  return (
    filterDecl(filter, decl, msg) &&
    filterSearched(filter.search, msg) &&
    filterKind(filter.kind, msg) &&
    filterEmitter(filter.emitter, msg)
  );
}

// --------------------------------------------------------------------------
// --- Filters panel and ratio
// --------------------------------------------------------------------------

function Checkbox(p: Forms.CheckboxFieldProps): JSX.Element {
  const lbl = p.label.charAt(0).toUpperCase() + p.label.slice(1).toLowerCase();
  return <Forms.CheckboxField label={lbl} title={p.title} state={p.state} />;
}

function MessageKindCheckbox(props: {
  kind: logkind,
  kindState: Forms.FieldState<KindFilter>,
}): JSX.Element {
  const { kind, kindState } = props;
  const state = Forms.useProperty(kindState, kind);
  return <Checkbox label={kind} state={state} />;
}

function PluginCheckbox(props: {
  plugin: string,
  title: string,
  pluginState: Forms.FieldState<PluginFilter>,
}): JSX.Element {
  const state = Forms.useProperty(props.pluginState, props.plugin);
  return <Checkbox title={props.title} label={props.plugin} state={state} />;
}


function MessageFilter(props: {
  filter: State<Filter>,
  plugins: Params.plugin[]
}): JSX.Element {
  const { filter, plugins } = props;
  const state = Forms.useValid(filter);
  const search = Forms.useProperty(state, 'search');
  const categoryState = Forms.useProperty(search, 'category');
  const messageState = Forms.useProperty(search, 'message');
  const kindState = Forms.useProperty(state, 'kind');
  const kindCheckboxes =
    Object.keys(kindFilter).map((k) => (
      <MessageKindCheckbox key={k} kind={k as logkind} kindState={kindState} />
    ));
  const emitterState = Forms.useProperty(state, 'emitter');
  const kernelState = Forms.useProperty(emitterState, 'kernel');
  const othersState = Forms.useProperty(emitterState, 'others');
  const pluginState = Forms.useProperty(emitterState, 'plugins');
  const emitterStateReset = React.useCallback(
    (val: boolean) => emitterState.onChanged({
        kernel: val,
        plugins: Object.fromEntries(plugins.map(p => [p.shortname, val])),
        others: val,
      }, false, true)
  , [emitterState, plugins]);
  const onContextMenu = React.useCallback(() => {
    const items: Dome.PopupMenuItem[] = [
      {
        label: 'Select all',
        onClick: () => emitterStateReset(true),
      },
      {
        label: 'Deselect all',
        onClick: () => emitterStateReset(false)
      },
    ];
    Dome.popupMenu(items);
  }, [emitterStateReset]);
  const pluginStateReady = Object.keys(pluginState.value).length > 0;
  const pluginCheckboxes = pluginStateReady && plugins.map((p) =>
    <PluginCheckbox key={p.shortname}
      title={p.name}
      plugin={p.shortname}
      pluginState={pluginState}
    />
  );

  return (
    <div className="message-search">
      <Forms.CheckboxField
        label="Current function"
        title="Only show messages emitted at the current function"
        state={Forms.useProperty(state, 'currentDecl')}
      />
      <Section label="Search"
        defaultUnfold={true}
        settings="message-filter-search"
      >
        <Forms.TextField
          label="Category"
          state={categoryState}
          placeholder="Category"
          title={'Search in message category.\n'
            + 'Use -<name> to hide some categories.'}
        />
        <Forms.TextField
          label="Message"
          state={messageState}
          placeholder="Message"
          title={'Search in message text.\n'
            + 'Case-insensitive by default.\n'
            + 'Use "text" for an exact case-sensitive search.'}
        />
      </Section>
      <Section label="Kind"
        defaultUnfold={true}
        settings="message-filter-kind"
      >
        {kindCheckboxes}
      </Section>
      <Section
        label="Emitter"
        defaultUnfold={true}
        settings="message-filter-emitter"
        rightButtonProps={{
          icon: 'TUNINGS',
          title: `Configure filters`,
          onClick: () => onContextMenu(),
        }}
      >
        <div className="message-emitter-category">
          <Forms.CheckboxField label='Kernel' state={kernelState} />
        </div>
        <div className="message-emitter-category">
          {pluginCheckboxes}
        </div>
        <div className="message-emitter-category">
          <Forms.CheckboxField label='Others' state={othersState} />
        </div>
      </Section>
    </div>
  );
}

function FilterRatio<K, R>(
  { model }: { model: Arrays.ArrayModel<K, R> }
): JSX.Element {
  const [filtered, total] = [model.getRowCount(), model.getTotalRowCount()];
  const title = `${filtered} displayed messages / ${total} total messages`;
  return (
    <Label className="component-info" title={title}>
      {filtered} / {total}
    </Label>
  );
}

// --------------------------------------------------------------------------
// --- Messages Columns
// --------------------------------------------------------------------------

const renderKind: Renderer<logkind> =
  (kind: logkind): JSX.Element => {
    const label = kind.toLocaleLowerCase();
    let icon = '';
    let color = 'black';
    switch (kind) {
      case 'RESULT': icon = 'ANGLE.RIGHT'; break;
      case 'FEEDBACK': icon = 'CIRC.INFO'; break;
      case 'DEBUG': icon = 'HELP'; break;
      case 'WARNING': icon = 'ATTENTION'; color = '#C00000'; break;
      case 'ERROR': case 'FAILURE': icon = 'WARNING'; color = '#C00000'; break;
    }
    return <Icon title={label} id={icon} fill={color} />;
  };

const renderCell: Renderer<string> =
  (text: string): JSX.Element => (<Cell title={text}>{text}</Cell>);

const renderMessage: Renderer<string> =
  (text: string): JSX.Element =>
    (<div title={text} className="message-cell"> {text} </div>);

const renderDir: Renderer<Ast.source> =
  (loc: Ast.source): JSX.Element =>
    (<Cell label={loc.dir} title={loc.file} />);

const renderFile: Renderer<Ast.source> =
  (loc: Ast.source): JSX.Element => (
    <Cell label={`${loc.base}:${loc.line}`} title={loc.file} />
  );

const renderDecl: Renderer<Ast.decl> =
  (decl: Ast.decl): JSX.Element => {
    const { name, label } = States.getDeclaration(decl);
    return <Cell label={name} title={label} />;
  };

const MessageColumns = (): JSX.Element => (
  <>
    <Column
      id="kind"
      title="Message kind"
      label="Kind"
      width={42}
      align="center"
      render={renderKind}
    />
    <Column
      id="plugin"
      label="Emitter"
      title="Frama-C kernel or plugin"
      width={75}
      render={renderCell}
    />
    <Column
      id="category"
      label="Category"
      title="Only for warning and debug messages"
      width={150}
      render={renderCell}
    />
    <Column
      id="message"
      label="Message"
      fill
      render={renderMessage}
    />
    <Column
      id="decl"
      label="Declaration"
      width={150}
      render={renderDecl}
    />
    <Column
      id="dir"
      label="Directory"
      width={240}
      visible={false}
      getter={(msg: Message) => msg?.source}
      render={renderDir}
    />
    <Column
      id="file"
      label="Location"
      width={150}
      getter={(msg: Message) => msg?.source}
      render={renderFile}
    />
    <Column
      id="key"
      label="Id"
      title="Message emission order"
      width={42}
      visible={false}
    />
  </>
);

// -------------------------------------------------------------------------
// --- Messages Table
// -------------------------------------------------------------------------

const bySource =
  Compare.byFields<Ast.source>({ file: Compare.alpha, line: Compare.number });

const byMessage: Compare.ByFields<Message> = {
  key: Compare.lift(parseInt, Compare.bignum),
  kind: Compare.structural,
  plugin: Compare.string,
  category: Compare.defined(Compare.string),
  decl: Compare.defined(Compare.alpha),
  source: Compare.defined(bySource),
};

const globalFilterState = new GlobalState(defaultFilter);

export function RenderMessages(): JSX.Element {

  const [model] = React.useState(() => {
    const f = (msg: Message): string => msg.key;
    const m = new Arrays.CompactModel<string, Message>(f);
    m.setOrderingByFields(byMessage);
    return m;
  });

  const data = States.useSyncArrayData(Kernel.message);

  React.useEffect(() => {
    model.removeAllData();
    model.updateData(data);
    model.reload();
  }, [model, data]);

  const filterState = useGlobalState(globalFilterState);
  const [filter, setFilter] = filterState;
  const selectedDecl = States.useCurrentScope();
  const [selectedMsg, selectMsg] = React.useState<Message>();
  const [text, setText] = React.useState('');

  /** List of plugins */
  const [plugins, setPlugins] = React.useState<Params.plugin[]>([]);
  React.useEffect(() => {
    const fetchPlugins = async (): Promise<void> => {
      const plugins = await Server.send(Params.getPlugins, {});
      setPlugins(plugins.filter(v => v.name !== "kernel")
        .sort((a, b) => Compare.alpha(a.name, b.name)));
    };
    if(Server.isRunning()) fetchPlugins();
    else Server.onReady(fetchPlugins);
  }, []);

  React.useEffect(() => {
    const current = globalFilterState.getValue();
    const newP = Object.fromEntries(plugins.map(p => [p.shortname, true]));

    const updated = {
      ...current,
      emitter: {
        ...current.emitter,
        plugins: newP
      }
    };

    setFilter(updated);
  }, [plugins, setFilter]);

  React.useEffect(() => {
    if (selectedDecl !== selectedMsg?.decl)
      selectMsg(undefined);
  }, [selectedDecl, selectedMsg?.decl]);

  React.useEffect(() => {
    model.setFilter((msg: Message) => filterMessage(filter, selectedDecl, msg));
  }, [model, filter, selectedDecl]);

  const onMessageSelection = React.useCallback(
    (msg: Message) => {
      selectMsg(msg);
      setText(msg.message);
      if (msg.decl || msg.marker) {
        States.setCurrentLocation({ scope: msg.decl, marker: msg.marker });
      }
    }, []
  );

  const [showFilter, flipFilter] =
    Dome.useFlipSettings('fc.kernel.messages.showFilter', true);

  const MessagePanel = (
    <Vbox style={{ height: '100%' }}>
      <IconButton
        icon="CROSS"
        title="Close"
        onClick={() => setText('')}
        style={{ margin: '0 auto' }}
      />
      <Scroll>
        <Page className="message-page"> {text} </Page>
      </Scroll>
    </Vbox>
  );

  return (
    <>
      <TitleBar help="framac-messages">
        <FilterRatio model={model} />
        <IconButton
          icon="CLIPBOARD"
          selected={showFilter}
          onClick={flipFilter}
          title="Toggle filters panel"
        />
      </TitleBar>
      <RSplit
        settings="fc.kernel.messages.filterSplit"
        defaultPosition={225}
        unfold={showFilter}
      >
        <BSplit
          settings="fc.kernel.messages.messageSplit"
          defaultPosition={90}
          unfold={text !== ''}
        >
          <Table<string, Message>
            model={model}
            sorting={model}
            selection={selectedMsg?.key}
            onSelection={onMessageSelection}
            settings="fc.kernel.messages.table"
          >
            <MessageColumns />
          </Table>
          {MessagePanel}
        </BSplit>
        <Scroll>
          <MessageFilter filter={filterState} plugins={plugins}/>
        </Scroll>
      </RSplit>
    </>
  );
}

// --------------------------------------------------------------------------
// --- Alerts
// --------------------------------------------------------------------------

let CURSOR = 0;

States.onSyncArray(Kernel.message, () => {
  const data = States.getSyncArrayData(Kernel.message);
  const curr = data.length < CURSOR ? 0 : CURSOR;
  let errors = 0;
  const plugins = new Set<string>();
  for (let k = curr; k < data.length; k++) {
    const m = data[k];
    switch (m.kind) {
      case Kernel.logkind.ERROR:
      case Kernel.logkind.FAILURE:
        errors++;
        plugins.add(m.plugin);
        break;
    }
  }
  CURSOR = data.length;
  const from = Array.from(plugins).join(',');
  const many = errors > 1 ? 's' : '';
  if (errors > 0) {
    Display.showError(`${errors} error${many} from ${from}`);
    Display.alertComponent('fc.kernel.messages');
  }
});

/* -------------------------------------------------------------------------- */
