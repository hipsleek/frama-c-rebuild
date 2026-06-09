/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Sandbox Testing for Diagram component                              --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import { Scroll } from 'dome/layout/boxes';
import { HSplit } from 'dome/layout/splitters';
import { Diagram, Node, Edge, Cluster } from 'dome/graph/diagram';
import { registerSandbox } from 'ivette';

// --------------------------------------------------------------------------
// --- Init functions for nodes and edges
// --------------------------------------------------------------------------

const nodes: Node[] = [
  { id: 'A' },
  { id: 'B', shape: 'diamond' },
  {
    id: 'R',
    shape: [
      { label: 'Dotted "c"', port: 'c' },
      ['D1', 'D2'],
      { label: 'Dashed "e"', port: 'e' },
    ]
  },
  { id: 'white', color: 'white', cluster: 'BG' },
  { id: 'grey', color: 'grey', cluster: 'BG' },
  { id: 'dark', color: 'dark', cluster: 'BG' },
  { id: 'primary', color: 'primary', cluster: 'BG' },
  { id: 'selected', color: 'selected', cluster: 'BG' },
  { id: 'green', color: 'green', cluster: 'BG' },
  { id: 'orange', color: 'orange', cluster: 'BG' },
  { id: 'red', color: 'red', cluster: 'BG' },
  { id: 'yellow', color: 'yellow', cluster: 'BG' },
  { id: 'blue', color: 'blue', cluster: 'BG' },
  { id: 'pink', color: 'pink', cluster: 'BG' },
  { id: 'X' }, { id: 'Y' }
];

const edges: Edge[] = [
  { source: 'A', target: 'R', targetPort: 'c', headLabel: 'c', line: 'dotted' },
  { source: 'R', target: 'B', sourcePort: 'e', tailLabel: 'e', line: 'dashed' },
  { source: 'primary', target: 'selected' },
  { source: 'white', target: 'grey' },
  { source: 'grey', target: 'dark' },
  { source: 'green', target: 'orange' },
  { source: 'orange', target: 'red' },
  { source: 'yellow', target: 'pink' },
  { source: 'pink', target: 'blue' },
  { source: 'white', target: 'white', color: 'white' },
  { source: 'grey', target: 'grey', color: 'grey' },
  { source: 'dark', target: 'dark', color: 'dark' },
  { source: 'primary', target: 'primary', color: 'primary' },
  { source: 'selected', target: 'selected', color: 'selected' },
  { source: 'green', target: 'green', color: 'green' },
  { source: 'orange', target: 'orange', color: 'orange' },
  { source: 'red', target: 'red', color: 'red' },
  { source: 'yellow', target: 'yellow', color: 'yellow' },
  { source: 'blue', target: 'blue', color: 'blue' },
  { source: 'pink', target: 'pink', color: 'pink' },
  {
    source: 'X', target: 'Y',
    tail: 'box', head: 'dot',
    label: '*', title: 'box to dot'
  },
];

function makeCluster(s: string | undefined): Cluster {
  const color = nodes.find(n => n.id === s)?.color;
  return {
    id: 'BG',
    title: 'Background Cluster',
    color: color ?? "default" };
}

function DiagramSample(): JSX.Element {
  const [model, setModel] = React.useState('');
  const [selected, setSelected] = React.useState<string>();
  const clusters = React.useMemo(() => [makeCluster(selected)], [selected]);
  return (
    <HSplit settings='sandbox.diagram.split'>
      <Scroll>
        <pre>Selected: {selected}</pre>
        <pre>
          {model}
        </pre>
      </Scroll>
      <Diagram
        nodes={nodes}
        edges={edges}
        clusters={clusters}
        selected={selected}
        onModelChanged={setModel}
        onSelection={setSelected}
      />
    </HSplit >
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.diagram',
  label: 'Diagram',
  preferredPosition: 'ABCD',
  children: <DiagramSample />,
});

// --------------------------------------------------------------------------
