/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Access List Renderer
// --------------------------------------------------------------------------

import React from 'react';
import { Hbox } from 'dome/layout/boxes';
import { Label, Cell, Code } from 'dome/controls/labels';
import * as Ast from 'frama-c/kernel/api/ast';
import * as States from 'frama-c/states';
import * as Region from './api';

/* -------------------------------------------------------------------------- */
/* --- Region Attributes                                                  --- */
/* -------------------------------------------------------------------------- */

function ACS(props: { mark: string; kind: string; acs: number }): JSX.Element {
  return (
    <Label
      display={props.acs > 0}
      title={`Number of {kind}s`}
    >
      {props.acs} {props.kind}{props.acs > 1 ? 's' : ''} ({props.mark})
    </Label>
  );
}

export interface AttributesProps {
  region?: Region.region;
}

export function Attributes(props: AttributesProps): JSX.Element | null {
  const { region } = props;
  if (!region) return null;
  const garbled =
    !region.typed &&
    !region.ranges.length &&
    (region.inits.length + region.reads.length + region.writes.length > 0);
  return (
    <Hbox>
      <Code icon="COMPONENT" className="dimmed">#{region.node}</Code>
      <ACS key="I" mark="I" kind="init" acs={region.inits.length} />
      <ACS key="R" mark="R" kind="read" acs={region.reads.length} />
      <ACS key="W" mark="W" kind="write" acs={region.writes.length} />
      <Label icon="WARNING" kind="negative" label="Garbled"
        title="Untyped region (multiple type access)"
        display={garbled} />
      <Label label={region.title} />
    </Hbox>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Access Lists                                                       --- */
/* -------------------------------------------------------------------------- */

interface AccessKind extends Region.access { kind: string }

const order = (a: AccessKind, b: AccessKind): number => {
  if (a.rank < b.rank) return -1;
  if (a.rank > b.rank) return +1;
  if (a.marker < b.marker) return -1;
  if (a.marker > b.marker) return +1;
  if (a.source < b.source) return -1;
  if (a.source > b.source) return +1;
  return 0;
};

function collect(r: Region.region | undefined): AccessKind[] {
  const buffer: AccessKind[] = [];
  if (r) {
    r.roots.forEach(r => buffer.push({
      rank: -1,
      kind: 'Region',
      access: r.range,
      typeof: r.typeof,
      source: r.attrs.join(', '),
      marker: r.marker,
    }));
    r.inits.forEach(r => buffer.push({ kind: 'Init', ...r }));
    r.reads.forEach(r => buffer.push({ kind: 'Read', ...r }));
    r.writes.forEach(r => buffer.push({ kind: 'Write', ...r }));
  }
  return buffer.sort(order);
}

interface AccessProps {
  access: AccessKind;
  selection: Ast.marker | undefined;
}

const fullWidth = { width: "100%" };

function Access(props: AccessProps): JSX.Element {
  const { access, selection } = props;
  const className = selection === access.marker ? "selected" : undefined;
  const onClick = (): void => States.setMarked(access.marker);
  return (
    <tr
      className={className}
      onClick={onClick}
      title={`Click to select source ${access.marker}`}
    >
      <td><Label
        label={access.kind}
        title="Access kind" /></td>
      <td><Cell
        className="dimmed"
        label={`( ${access.typeof} )`}
        title="Type of accessed value" /></td>
      <td><Cell
        label={access.access}
        title="Accessed expression, l-value or term" /></td>
      <td><Cell
        label={access.source}
        title="Origin or property" /></td>
      <td style={fullWidth} />
    </tr>
  );
}

export interface AccessListProps {
  region?: Region.region;
  selection?: Ast.marker;
}

export function AccessList(props: AccessListProps): JSX.Element | null {
  const { region, selection } = props;
  const acs = React.useMemo(() => collect(region), [region]);
  if (!acs.length) return null;
  return (
    <table className="wp-access-list">
      <tbody>
        {acs.map((a, k) => <Access key={k} access={a} selection={selection} />)}
      </tbody>
    </table>
  );
}

// --------------------------------------------------------------------------
