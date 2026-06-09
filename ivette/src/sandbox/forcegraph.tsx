/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

/* -------------------------------------------------------------------------- */
/* --- Sandbox Testing for Force Graph component                          --- */
/* -------------------------------------------------------------------------- */

import React from 'react';
import { Code } from 'dome/controls/labels';
import { ToolBar, ButtonGroup, Button, Filler } from 'dome/frame/toolbars';
import { Graph, Node, Edge, Layout } from 'dome/graph/graph';
import { registerSandbox } from 'ivette';

// --------------------------------------------------------------------------
// --- Init functions for nodes and edges
// --------------------------------------------------------------------------

let Kid = 0;
const random = (n: number): number => Math.floor(Math.random() * n);

function addNode(nodes: readonly Node[]): readonly Node[] {
  const k = Kid++;
  return nodes.concat({ id: `N${k}`, label: `Node #${k}` });
}

function addEdge(
  nodes: readonly Node[],
  edges: readonly Edge[]
): readonly Edge[] {
  const n = nodes.length;
  if (n <= 2) return edges;
  const source = nodes[random(n)].id;
  const target = nodes[random(n)].id;
  return edges.concat({ source, target });
}

// --------------------------------------------------------------------------
// --- Main force graph component
// --------------------------------------------------------------------------

function GraphSample(): JSX.Element {
  // Set initial configs
  const [nodes, setNodes] = React.useState<readonly Node[]>([]);
  const [edges, setEdges] = React.useState<readonly Edge[]>([]);
  const [layout, setLayout] = React.useState<Layout>('2D');
  const [selected, setSelected] = React.useState<string>();

  return (
    <>
      <ToolBar>
        <ButtonGroup>
          <Button
            label='2D'
            selected={layout === '2D'}
            onClick={() => setLayout('2D')} />
          <Button
            label='3D'
            selected={layout === '3D'}
            onClick={() => setLayout('3D')} />
        </ButtonGroup>
        <Code>Selected: {selected ?? '-'}</Code>
        <Filler/>
        <Code>Nodes: {nodes.length} Edges: {edges.length}</Code>
        <Button
          icon='CIRC.PLUS'
          label='Node'
          onClick={() => setNodes(addNode(nodes))}
        />
        <Button
          icon='CIRC.PLUS'
          label='Edge'
          onClick={() => setEdges(addEdge(nodes, edges))}
        />
        <Button
          icon='CIRC.CLOSE' kind='negative'
          label='Clear'
          onClick={() => {
            setEdges([]);
            setNodes([]);
            setSelected(undefined);
          }}
        />
      </ToolBar>
      <Graph
        nodes={nodes}
        edges={edges}
        layout={layout}
        selected={selected}
        onSelection={setSelected}
      />
    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Sandbox                                                            --- */
/* -------------------------------------------------------------------------- */

registerSandbox({
  id: 'sandbox.graph',
  label: 'Graph 2D/3D',
  preferredPosition: 'ABCD',
  children: <GraphSample />,
});

// --------------------------------------------------------------------------
