/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

// --------------------------------------------------------------------------
// --- Memory Region View
// --------------------------------------------------------------------------

import React from 'react';
import * as Dot from 'dome/graph/diagram';
import * as Region from './api';

// --------------------------------------------------------------------------
// --- Dot Diagram Builder
// --------------------------------------------------------------------------

function makeRecord(
  edges: Dot.Edge[],
  source: string,
  sizeof: number,
  ranges: Region.range[]
): Dot.Cell[] {
  if (ranges.length === 0) return [];
  const cells: Dot.Cell[] = [];
  let offset = 0;
  ranges.forEach((rg, i) => {
    const port = `P${i}`;
    const target = `n${rg.data}`;
    edges.push({
      source, sourcePort: port, target,
      head: 'none', line: 'dashed'
    });
    if (offset !== rg.offset)
      cells.push(`#${rg.offset - offset}b`);
    offset = rg.offset + rg.length;
    const label = rg.label;
    cells.push({ label, port });
  });
  if (offset !== sizeof)
    cells.push(`#${sizeof - offset}b`);
  return cells;
}

interface Diagram {
  nodes: readonly Dot.Node[];
  edges: readonly Dot.Edge[];
  index: Map<string, Region.node>;
  target: Map<Region.node, string>;
}

function makeDiagram(regions: readonly Region.region[]): Diagram {
  const nodes: Dot.Node[] = [];
  const edges: Dot.Edge[] = [];
  const index = new Map<string, Region.node>();
  const target = new Map<Region.node, string>();
  regions.forEach(r => {
    const id = `n${r.node}`;
    // --- Color
    const rd = r.reads.length > 0;
    const wr = r.writes.length > 0;
    const color =
      (!wr && !rd) ? undefined :
        !r.typed ? 'red' :
          r.pointed !== undefined
            ? (wr ? 'orange' : 'yellow')
            : (wr && rd) ? 'green' :
              wr ? 'pink' : 'grey';
    // --- Shape
    const font = r.ranges.length > 0 ? 'mono' : 'sans';
    const cells = makeRecord(edges, id, r.sizeof, r.ranges);
    const shape = cells.length > 0 ? cells : undefined;
    nodes.push({ id, font, color, label: r.label, title: r.title, shape });
    index.set(id, r.node);
    target.set(r.node, id);
    // --- Labels
    const L: Dot.Node =
      { id: '', shape: 'note', font: 'mono', color: 'lightgrey' };
    if (r.labels.length > 0) {
      const lid = `L${r.node}`;
      nodes.push({ ...L, id: lid, label: `${r.labels.join(',')}:` });
      index.set(lid, r.node);
      edges.push({
        source: lid, target: id, aligned: true,
        headAnchor: 's', head: 'none', color: 'grey'
      });
    }
    // --- Roots
    const R: Dot.Node = { id: '', shape: 'cds', font: 'mono' };
    // --- Roots: Result
    if (r.result) {
      const rid = '\\result';
      nodes.push({ ...R, id: rid, label: rid, title: "Returned value" });
      index.set(rid, r.node);
      edges.push({
        source: rid, target: id,
        headAnchor: "e", head: 'none', color: 'grey'
      });
    }
    // --- Roots: Variables
    r.cvars.forEach(x => {
      const xid = `X${x.name}`;
      nodes.push({ ...R, id: xid, label: x.label, title: x.title });
      index.set(xid, r.node);
      edges.push({
        source: xid, target: id,
        headAnchor: "e", head: 'none', color: 'grey'
      });
    });
    // --- Roots: Array Ranges
    const A: Dot.Node = { ...R, color: 'blue' };
    r.roots.forEach((a, k) => {
      const aid = `A${r.node}#${k}`;
      nodes.push({ ...A, id: aid, label: a.range, title: a.typeof });
      index.set(aid, r.node);
      edges.push({
        source: aid, target: id,
        headAnchor: "e", head: 'none', color: 'grey'
      });
    });
    // --- Pointed
    if (r.pointed !== undefined) {
      const pid = `n${r.pointed}`;
      edges.push({ source: id, target: pid });
    }
  });
  return { nodes, edges, index, target };
}

function addSelected(
  diag: Diagram,
  node: Region.node | undefined,
  label: string | undefined
): Diagram {
  if (node && label) {
    const sid = '\\selected';
    const nodes = diag.nodes.concat({
      id: sid, label, title: "Selected Marker",
      shape: 'note', color: 'selected'
    });
    diag.index.set(sid, node);
    const edges = diag.edges.concat({
      source: sid, target: `n${node}`, aligned: true,
      headAnchor: 's', tailAnchor: 'n',
    });
    return { ...diag, nodes, edges };
  } else
    return diag;
}

export interface MemoryViewProps {
  regions?: readonly Region.region[];
  localized?: Region.node;
  selected?: Region.node;
  label?: string;
  onSelection?: (node: Region.node | undefined) => void;
  onModelChanged?: (dot:string) => void;
}

export function MemoryView(props: MemoryViewProps): JSX.Element {
  const { regions = [], label, selected, localized, onSelection } = props;
  const baseDiagram = React.useMemo(() => makeDiagram(regions), [regions]);
  const fullDiagram = React.useMemo(
    () => addSelected(baseDiagram, localized, label),
    [baseDiagram, localized, label]
  );
  const { index, target } = baseDiagram;
  const selectedId = selected !== undefined ? target.get(selected) : undefined;
  const onSelectionId = React.useCallback(
    (id: string | undefined) =>
      onSelection && onSelection(id ? index.get(id) : undefined),
    [index, onSelection]
  );
  const { nodes, edges } = fullDiagram;
  return (
    <Dot.Diagram
      selected={selectedId}
      onSelection={onSelectionId}
      onModelChanged={props.onModelChanged}
      nodes={nodes}
      edges={edges}
    />
  );
}

// --------------------------------------------------------------------------
