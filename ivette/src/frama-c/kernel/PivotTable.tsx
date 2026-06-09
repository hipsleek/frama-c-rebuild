/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Pivot Table
// --------------------------------------------------------------------------

import React from 'react';
import * as Dome from 'dome';
import { Button, IconButton } from 'dome/controls/buttons';
import { LED } from 'dome/controls/displays';
import { Scroll } from 'dome/layout/boxes';
import { GlobalState, useGlobalState } from 'dome/data/states';
import { BSplit } from 'dome/layout/splitters';
import * as Models from 'dome/table/models';
import * as Arrays from 'dome/table/arrays';
import { Column, Renderer, Table } from 'dome/table/views';
import { Label } from 'dome/controls/labels';

import { TitleBar } from 'ivette';
import * as Display from 'ivette/display';

import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as PivotState from 'frama-c/plugins/pivot/api/general';
import PivotTableUI from 'react-pivottable/PivotTableUI';
import 'frama-c/kernel/PivotTable-style.css';


// --------------------------------------------------------------------------
// --- Pivot Table for Properties
// --------------------------------------------------------------------------

interface PivotTableProps {
  data: string[][];
  setFilterTable: (v: {[key: string]: string}) => void;
}

const PivotGlobalState = new GlobalState({});

export function Pivot(props: PivotTableProps): JSX.Element {
  const [state, setState] = useGlobalState(PivotGlobalState);
  return (
    <PivotTableUI
      data={props.data}
      onChange={setState}
      {...state}
      tableOptions={{
        clickCallback: (_e: Event, _value: number,
          filters: {[key: string]: string}
        ) => {
          props.setFilterTable(filters);
        }
      }}
    />
  );
}

function PivotTable(
  rawData: PivotState.tableStateType,
  setFilterTable: (v: {[key: string]: string}) => void
): JSX.Element {
  const data = new Array(rawData.length > 0 ? rawData.length - 1 : 0);
  if (rawData.length > 0) {
    const headers = rawData[0];
    for (let i = 1; i < rawData.length; i++) {
      const src = rawData[i];
      data[i - 1] = {};
      for (let j = 0; j < headers.length; j++) {
        data[i - 1][headers[j]] = src[j];
      }
    }
  }
  return (<Pivot data={data} setFilterTable={setFilterTable}/>);
}

function PivotTableBuild(
  { rawData, setFilter }: {
    rawData?: PivotState.tableStateType,
    setFilter: (v: {[key: string]: string}) => void
  }
): JSX.Element {
  const [computing, setComputing] = React.useState(false);
  const [error, setError] = React.useState('');
  async function handleError(err: string): Promise<void> {
    const label = 'Pivot Table Error';
    const title = `Building error (${err})`;
    Display.showError({ label, title });
  }
  async function compute(): Promise<void> {
    setComputing(true);
    setError('');
    Server.send(PivotState.compute, [])
      .then(() => setComputing(false))
      .catch((err) => { setComputing(false); handleError(err); });
  }
  if (rawData && rawData.length > 0) {
    return (PivotTable(rawData, setFilter));
  }
  if (computing) {
    return (
      <div className="pivot-centered">
        <div>
          <LED status="active" blink /> Computing…
        </div>
      </div>
    );
  }
  const err = error ? <div className="part"> {error} </div> : undefined;
  return (
    <div className="pivot-centered">
      {err}
      <div className="part">
        <Button
          icon="EXECUTE"
          label="Compute"
          title="Builds the pivot table. This may take a few moments."
          onClick={compute}
        />
      </div>
    </div>
  );
}

function onPresetMenu(set: (a: Preset) => void): void {
  const items: Dome.PopupMenuItem[] = Object.entries(presetList)
    .map(([name, preset]) => {
      return { label: name, onClick: () => set(preset) };
    });
  Dome.popupMenu(items);
}


// /** Table */

interface TableProps {
  model: Arrays.CompactModel<string, string[]>;
  table?: PivotState.tableStateType;
  filter?: {[key: string]: string};
}

const renderLabel: Renderer<string> = (val: string): JSX.Element | null => {
    return (val ? <Label label={val} /> : null);
  };

function PivotTableValue(props: TableProps): React.ReactNode {
  const { table, filter, model } = props;
  const columns = table ? table[0] : undefined;
  const datas = table?.slice(1);

  const headerObject = React.useMemo(() => {
    if(!columns) return undefined;
    return Object.fromEntries(columns.map((v, i) => [v, i]));
  }, [columns]);

  const filterTable = React.useCallback((row: string[]): boolean => {
    if(!headerObject || !filter || Object.entries(filter).length === 0)
      return false;
    let ret = true;
    Object.entries(filter).forEach(([k, v]) => {
      if(row[headerObject[k]] !== v) ret = false;
    });
    return ret;
  }, [filter, headerObject]);

  React.useEffect(() => {
    model.removeAllData();
    datas && datas.forEach((data, i) => {
      const key = i.toString();
      model.setData(key, { ...data });
    });
    model.reload();
  }, [datas, model]);

  React.useEffect(() => {
    model.setFilter(filterTable);
    model.reload();
  }, [model, filterTable]);

  if(!columns) return null;
  return <Table<string, string[]>
      model={model}
      sorting={model}
      settings="ivette.pivotTable.table"
    ><>
      { columns && columns.map((e, i) => <Column
          key={i}
          id={e}
          label={e}
          width={240}
          visible={e !== 'key' }
          getter={(prop: string[]) => prop[i]}
          render={renderLabel}
          />
        )
      }
    </></Table>;
}

function FilterRatio(
  { model }: { model: Arrays.CompactModel<string, string[]> }
): JSX.Element {
  Models.useModel(model);
  const [filtered, total] = [model.getRowCount(), model.getTotalRowCount()];
  return (
    <Label className="component-info" title="filtered / Total">
      { filtered} / {total }
    </Label>
  );
}

export default function PivotTableComponent(): JSX.Element {
  const rawData = States.useSyncValue(PivotState.pivotState);
  const [state, setState] = useGlobalState(PivotGlobalState);
  const [showDatas, flipShowDatas] =
    Dome.useFlipSettings('ivette.pivottable.showdatas', true);
  const [currentPreset, setCurrentPreset] = React.useState<Preset>();
  const filterTableState = React.useState<{[key: string]: string}>({});
  const [ filterTable, ] = filterTableState;
  // Pas de sens cette clé voir si on peut mettre un marker de l'AST
  const model = new Arrays.CompactModel<string, string[]>(
    (v: string[]) => v[0]);

  React.useEffect(() => {
    if (currentPreset) {
      const v = PivotGlobalState.getValue();
      setState({ ...v, ...currentPreset });
      setCurrentPreset(undefined);
    }
  }, [setState, currentPreset]);

  React.useEffect(() => {
    if (rawData && rawData.length > 0) {
      const v = PivotGlobalState.getValue();
      setState({ rawData, ...v });
    }
  }, [rawData, setState]);

  return (
    <>
      <TitleBar label='Pivot Table'>
        <FilterRatio model={model} />
        { Object.entries(state).length > 0 &&
          <IconButton
            icon='TUNINGS'
            title="Select Preset"
            onClick={() => onPresetMenu(setCurrentPreset)}
          />
        }
        <IconButton
          icon="CLIPBOARD"
          selected={showDatas}
          onClick={flipShowDatas}
          title="Toggle datas panel"
        />
      </TitleBar>
      <BSplit
        settings="ivette.pivottable.tableBSplit"
        defaultPosition={200}
        unfold={showDatas && !!rawData && rawData.length > 0}
      >
        <Scroll>
          <PivotTableBuild
            rawData={rawData}
            setFilter={filterTableState[1]}
          />
        </Scroll>
        <PivotTableValue
          model={model}
          table={rawData}
          filter={filterTable}
        />
      </BSplit>
    </>
  );
}

/** Preset list */
interface Preset {
  rows: string[],
  cols: string[],
  valueFilter: {[key: string]: {[key:string]: boolean}}
}

const presetList: {[name: string]: Preset} = {
  "Syntax - Code - Statements per function": {
    rows: ['Filename', 'Function'],
    cols: ['Node'],
    valueFilter: {
      Filename: { "<unknown location>": true },
      Domain: { message: true, property: true },
      Kind: {
        annot: true,
        debug: true,
        decl: true,
        feedback: true,
        result: true,
        warning: true,
        '': true
      }
    }
  },
  "Syntax - Code - Annotations per function": {
    rows: ['Filename', 'Function'],
    cols: ['Node'],
    valueFilter: {
      Filename: { "<unknown location>": true },
      Domain: { message: true, property: true },
      Kind: {
        code: true,
        debug: true,
        decl: true,
        feedback: true,
        result: true,
        warning: true,
        '': true
      }
    }
  },
  "Messages - Warnings/Errors per function": {
    rows: ['Filename', 'Function'],
    cols: ['Node', 'Kind'],
    valueFilter: {
      Filename: { "<unknown location>": true },
      Domain: { syntax: true, property: true },
      Kind: {
        annot: true,
        code: true,
        debug: true,
        decl: true,
        feedback: true,
        result: true,
        '': true
      }
    }
  },
  "Properties - Unknown/Invalid Statuses per function": {
    rows: ['Filename', 'Function'],
    cols: ['Node', 'Status'],
    valueFilter: {
      Filename: { "<unknown location>": true },
      Domain: { syntax: true, message: true },
      Status: {
        "Never_tried": true,
        "Considered_valid": true,
        "Valid": true,
        "Valid_under_hyp": true,
        "Valid_but_dead": true,
      }
    }
  }
};

// --------------------------------------------------------------------------
