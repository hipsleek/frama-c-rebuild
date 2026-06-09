/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { Table, Column } from 'dome/table/views';
import * as Arrays from 'dome/table/arrays';
import * as Compare from 'dome/data/compare';
import * as Ivette from 'ivette';
import * as States from 'frama-c/states';
import * as EvaStats from 'frama-c/plugins/eva/api/stats';

import CoverageMeter, { percent } from './CoverageMeter';
import { EvaStatus } from './components/AnalysisStatus';

type stats = EvaStats.functionStatsData;

// --- Coverage Table ---

const ordering: Arrays.ByColumns<stats> = {
  fct: Compare.byFields({ fctName: Compare.string }),
  alarms: Compare.byFields({
    alarmStatuses: Compare.lift(
      (x) => x.unknown + x.invalid,
      Compare.number,
    ),
  }),
  sureAlarms: Compare.byFields(
    { alarmStatuses: Compare.byFields({ invalid: Compare.number }) },
  ),
  deadStatements: Compare.byFields({
    coverage: Compare.byFields(
      { dead: Compare.number },
    ),
  }),
  reachableStatements: Compare.byFields({
    coverage: Compare.byFields(
      { reachable: Compare.number },
    ),
  }),
  totalStatements: Compare.byFields(
    { coverage: Compare.lift((x) => x.reachable + x.dead, Compare.number) },
  ),
  coverage: Compare.byFields(
    {
      coverage: Compare.lift(
        (x) => x.reachable / (x.reachable + x.dead),
        Compare.number,
      ),
    },
  ),
};

export function CoverageTable(): JSX.Element {

  const model = States.useSyncArrayModel(EvaStats.functionStats);
  React.useEffect(() => {
    model.setColumnOrder(ordering);
    model.setSorting({ sortBy: 'coverage', sortDirection: 'ASC' });
  }, [model]);

  const selection = States.useCurrentScope();
  const onSelection = (s: stats): void => States.setCurrentScope(s.key);

  return (
    <Table
      model={model}
      sorting={model}
      selection={selection}
      onSelection={onSelection}
      settings="ivette.coverage.table"
    >
      <Column
        id="fct"
        label="Function"
        align="left"
        width={200}
        fill
        getter={(s: stats) => s.fctName}
      />
      <Column
        id="alarms"
        label="Alarms"
        title="Number of alarms emitted by the Eva analysis"
        align="center"
        width={80}
        getter={(s: stats) =>
          s.alarmStatuses.invalid + s.alarmStatuses.unknown}
      />
      <Column
        id="sureAlarms"
        label="Sure alarms"
        title="Number of sure alarms emitted by the Eva analysis"
        align="center"
        width={80}
        getter={(s: stats) => s.alarmStatuses.invalid}
      />
      <Column
        id="deadStatements"
        label="Dead"
        title="Number of statements unreachable to the Eva analysis"
        align="center"
        visible={false}
        width={80}
        getter={(s: stats) => s.coverage.dead}
      />
      <Column
        id="reachableStatements"
        label="Reachable"
        title="Number of statements reached by the Eva analysis"
        align="center"
        visible={false}
        width={80}
        getter={(s: stats) => s.coverage.reachable}
      />
      <Column
        id="totalStatements"
        label="Total"
        title="Total number of statements"
        align="center"
        visible={false}
        width={80}
        getter={ (s: stats) => s.coverage.dead + s.coverage.reachable}
      />
      <Column
        id="coverage"
        label="Coverage"
        title="Coverage of the Eva analysis"
        align="center"
        width={80}
        getter={(s: stats) => s.coverage}
        render={(coverage) => <>{percent(coverage)}</>}
      />
      <Column
        id="coverage-meter"
        label=""
        align="center"
        width={80}
        getter={(s: stats) => s.coverage}
        render={(coverage) => <CoverageMeter coverage={coverage} />}
      />
    </Table>
  );
}

// --- Coverage Component ---

export default function CoverageComponent(): JSX.Element {
  return (
    <>
      <Ivette.TitleBar help="eva-coverage">
        <EvaStatus />
      </Ivette.TitleBar>
      <CoverageTable />
    </>
  );
}

Ivette.registerComponent({
  id: 'fc.eva.coverage',
  label: 'Eva Coverage',
  title: 'Detailed coverage of the Eva analysis',
  children: <CoverageComponent />,
});

// --------------------------------------------------------------------------
