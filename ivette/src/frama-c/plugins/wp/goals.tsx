/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { Icon } from 'dome/controls/icons';
import { IconKind, Cell, Descr } from 'dome/controls/labels';
import { Filter } from 'dome/table/models';
import { Table, Column } from 'dome/table/views';
import * as States from 'frama-c/states';
import * as Ast from 'frama-c/kernel/api/ast';
import * as WP from 'frama-c/plugins/wp/api';
import * as Locations from 'frama-c/kernel/Locations';

/* -------------------------------------------------------------------------- */
/* --- Table Cells                                                        --- */
/* -------------------------------------------------------------------------- */

interface IconProps {
  icon?: string;
  title?: string;
}

function renderIcon(s : IconProps): JSX.Element {
  const { icon=' ', title } = s;
  return <Icon id={icon} title={title} />;
}

interface CellProps {
  icon: string;
  label: string;
  kind: IconKind;
  title: string;
}

function renderCell(s : CellProps): JSX.Element {
  return <Cell {...s} />;
}

/* -------------------------------------------------------------------------- */
/* --- Scope Column                                                       --- */
/* -------------------------------------------------------------------------- */

function getScope(g : WP.goalsData): string {
  if (g.bhv && g.fct) return `${g.fct} — {g.bhv}}`;
  if (g.fct) return g.fct;
  if (g.thy) return g.thy;
  return 'Global';
}

/* -------------------------------------------------------------------------- */
/* --- Script Column                                                      --- */
/* -------------------------------------------------------------------------- */

/* eslint-disable max-len */
const savedScript: IconProps = { icon: 'FOLDER', title: 'Saved Script' };
const updatedScript: IconProps = { icon: 'FOLDER.OPEN', title: 'Updated Script' };
const proofEdit: IconProps = { icon: 'CODE', title: 'Proof Under Construction' };
const proofNone: IconProps = { title: 'No Proof Script' };
/* eslint-enable max-len */

export function getScript(g : WP.goalsData): IconProps {
  const { script, saved, proof } = g;
  return (
    script ? (saved ? savedScript : updatedScript)
      : (proof ? proofEdit : proofNone)
  );
}

/* -------------------------------------------------------------------------- */
/* --- Status Column                                                      --- */
/* -------------------------------------------------------------------------- */

const noResult : CellProps =
  { icon: 'MINUS', label: 'No Result', kind: 'disabled', title: 'No Result' };

interface BaseProps {
  icon: string;
  label: string;
  kind: IconKind;
  title: string;
}

/* eslint-disable max-len */
const baseStatus : { [key:string]: BaseProps } = {
  'VALID': { icon: 'CHECK', label: 'Valid', kind: 'positive', title: 'Valid Goal' },
  'PASSED': { icon: 'CHECK', label: 'Passed', kind: 'positive', title: 'Passed Test' },
  'DOOMED': { icon: 'CROSS', label: 'Doomed', kind: 'negative', title: 'Doomed Test' },
  'FAILED': { icon: 'WARNING', label: 'Failed', kind: 'negative', title: 'Prover Failure' },
  'UNKNOWN': { icon: 'ATTENTION', label: 'Unknown', kind: 'warning', title: 'Prover Stuck' },
  'TIMEOUT': { icon: 'HELP', label: 'Timeout', kind: 'warning', title: 'Prover Timeout' },
  'STEPOUT': { icon: 'HELP', label: 'Stepout', kind: 'warning', title: 'Prover Stepout' },
  'COMPUTING': { icon: 'EXECUTE', label: 'Running', kind: 'default', title: 'Prover is running' },
};
/* eslint-enable max-len */

export function getStatus(g : WP.goalsData): CellProps {
  const { label, ...base } = baseStatus[g.status] ?? noResult;
  return { ...base, label: label + g.stats.summary };
}

/* -------------------------------------------------------------------------- */
/* --- Goals Filter                                                       --- */
/* -------------------------------------------------------------------------- */

function filterGoal(
  failed: boolean,
  scope: Ast.decl | undefined,
): Filter<WP.goal, WP.goalsData> {
  return (goal: WP.goalsData): boolean => {
    if (failed && goal.passed) return false;
    if (scope && goal.scope !== scope) return false;
    return true;
  };
}

/* -------------------------------------------------------------------------- */
/* --- Goals Table                                                        --- */
/* -------------------------------------------------------------------------- */

type Goal = WP.goal | undefined;

export interface GoalTableProps {
  display: boolean;
  failed: boolean;
  scoped: boolean;
  scope: Ast.decl | undefined;
  current: WP.goal | undefined;
  setCurrent: (goal: WP.goal | undefined) => void;
  setTIP: (goal: WP.goal) => void;
  setGoals: (goals: number) => void;
  setTotal: (total: number) => void;
}

export function GoalTable(props: GoalTableProps): JSX.Element {
  const {
    display, scoped, failed,
    scope,
    current, setCurrent,
    setTIP,
    setGoals, setTotal,
  } = props;
  const { model } = States.useSyncArrayProxy(WP.goals);

  const goals = model.getRowCount();
  const total = model.getTotalRowCount();

  const selectedMarker = States.getSelected();
  const markerGoals =
    States.useRequestResponse(WP.getGoalsFromASTMarker, selectedMarker);

  const [target, setTarget] = React.useState<Goal>(undefined);

  const candidates =
    markerGoals?.filter(
      (value: WP.goal): boolean => !failed || !model.getData(value)?.passed
    );

  React.useEffect(() => {
    if (candidates) {
      const selection =
        candidates.length === 0
          ? undefined
          : target && candidates.includes(target)
            ? target
            : candidates[0];

      setCurrent(selection);
    }
  }, [target, setCurrent, candidates]);

  const onSelection = React.useCallback(
    ({ wpo, marker }: WP.goalsData) => {
      States.setSelected(marker);
      setTarget(wpo);
    }, []);

  const onDoubleClick = React.useCallback(
    ({ wpo }: WP.goalsData) => {
      setTIP(wpo);
    }, [setTIP]
  );

  React.useEffect(() => {
    if (failed || scoped) {
      /* if we ever add new filters here, check selection above */
      model.setFilter(filterGoal(failed, scope));
    } else {
      model.setFilter();
    }
  }, [model, failed, scoped, scope]);

  React.useEffect(() => setGoals(goals), [goals, setGoals]);
  React.useEffect(() => setTotal(total), [total, setTotal]);

  React.useEffect(() => {
    if (current) {
      const data = model.getData(current);
      if (data) {
        const { name, deps } = data;
        Locations.setSelection({
          plugin: 'WP',
          label: `Dependencies of ${name}`,
          title: `${name} depends on these statements and annotations`,
          markers: deps
        });
      }
    } else {
      Locations.clearSelection();
    }
  }, [current, model]);

  const renderEmpty = React.useCallback(() => {
    const kind = failed ? ' failed' : '';
    const loc = scoped ? ' in current scope' : '';
    const icon = scoped ? 'CURSOR' : failed ? 'CIRC.INFO' : 'INFO';
    return (
      <Descr
        className='wp-empty-goals'
        icon={icon} label={`No${kind} goals${loc}`} />
    );
  }, [scoped, failed]);

  return (
    <Table
      model={model}
      display={display}
      settings='wp.goals'
      selection={current}
      onSelection={onSelection}
      onDoubleClick={onDoubleClick}
      renderEmpty={renderEmpty}
    >
      <Column
        id='scope'
        label='Scope'
        width={150}
        getter={getScope} />
      <Column
        id='name'
        label='Property'
        width={150} />
      <Column
        id='script'
        icon='FILE'
        fixed width={30}
        getter={getScript}
        render={renderIcon} />
      <Column
        id='status'
        label='Status'
        fill={true}
        getter={getStatus}
        render={renderCell} />
    </Table>
  );
}

/* -------------------------------------------------------------------------- */
