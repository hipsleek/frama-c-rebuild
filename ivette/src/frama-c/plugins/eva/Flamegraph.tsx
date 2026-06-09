/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import AutoSizer, { Size } from 'react-virtualized-auto-sizer';
import flamegraph from "d3-flame-graph";
import { select, Selection } from 'd3-selection';

import { IconButton } from 'dome/controls/buttons';
import { Inset } from 'dome/frame/toolbars';
import { useFlipSettings } from 'dome';

import * as Ivette from 'ivette';

import * as States from 'frama-c/states';
import * as EvaStats from 'frama-c/plugins/eva/api/stats';

import { EvaReady, EvaStatus } from './components/AnalysisStatus';

// --- Flamegraph Table ---
interface Flamegraph {
  name: string;
  value: number;
  children: Flamegraph[];
  info?: EvaStats.flamegraphData;
}

const addNodeToFlamegraph = (
  flamegraph: Flamegraph,
  indexCs: number,
  row: EvaStats.flamegraphData,
): void => {
  /* Accumulate times for all nodes crossed. We do not rely on [row.totalTime]
     as during the analysis, the flamegraph is incomplete and the total time
     of some callstacks may be inconsistent. So we rebuild the total time of
     each callstack from the selfTime of all available callstacks. */
  flamegraph.value += row.selfTime;
  // updating last node
  if(indexCs === row.stackNames.length) {
    flamegraph.info = row;
  } else {
    // Search/create next node
    let nextNode = flamegraph.children.find(
      (elt) => elt.name === row.stackNames[indexCs]);
    if (!nextNode) {
      nextNode = { name: row.stackNames[indexCs], value: 0, children: [] };
      flamegraph.children.unshift(nextNode);
    }
    // Treatment of the next node
    addNodeToFlamegraph(nextNode, indexCs+1, row);
  }
};

/* Round f to at most [decimal] decimals. */
function round(f: number, decimal: number): number {
  const factor = 10 ** decimal;
  return Math.round(f * factor) / factor;
}

/* Returns text to be shown about a node in a flamegraph. */
function nodeInfoText(flameGraph:Flamegraph, node:Flamegraph): string {
  if (node.info === undefined) return "";
  const percentage = round(100 * node.value / flameGraph.value, 1);
  const total = round(node.value, 2);
  const self = round(node.info.selfTime, 2);
  const infos =
    `${node.name}:\n`
    + `  callstack analyzed ${node.info.nbCalls} times\n`
    + `  total time (including called functions): ${total}s.,  ${percentage}%\n`
    + `  time for ${node.name} only: ${self}s.`;
  return infos;
}

// --- Flamegraph Component ---
export type FlameNode = { data: Flamegraph; };

type Container = Selection<HTMLDivElement, FlameNode, null, undefined>;

interface EvaFlamegraphProps {
  useScope: boolean;
  data: Flamegraph;
  size: Size
}

export function EvaFlamegraph(props: EvaFlamegraphProps): JSX.Element {
  const { useScope, data, size } = props;
  const { width, height } = size;
  const [ nodeInfos, setNodeInfos ] = React.useState("");
  const ref = React.useRef<HTMLDivElement | null>(null);

  React.useEffect(() => {
    if (!ref.current) return;

    const container: Container = select(ref.current);

    function attachEvents(container: Container): void {
      container
        .selectAll<SVGRectElement, FlameNode>(".frame")
        .on("mouseover", (_e: Event, node: FlameNode) => {
          setNodeInfos(nodeInfoText(data, node.data));
        })
        .on("mouseout", () => setNodeInfos(""));
    }

    const chart = flamegraph()
      .width(width)
      .cellHeight(20)
      .inverted(true)
      .transitionDuration(350)
      .onClick((node: FlameNode) => {
        if (useScope) States.setCurrentScope(node.data.info?.kfDecl);
        attachEvents(container);
      });

    container.datum(data).call(chart);
    // Remove tooltips
    container.selectAll("title").remove();
    attachEvents(container);
  }, [data, width, height, useScope]);


  return <>
    <div ref={ref} />
    {
      nodeInfos &&
      <div className='flame-details'>
        {nodeInfos}
      </div>
    }
  </>;
}

// --- Flamegraph Component ---
export function FlamegraphComponent(): JSX.Element {
  const [useScope, flipUseScope] =
    useFlipSettings("eva.flamegraph.scope", true);
  const model = States.useSyncArrayData(EvaStats.flamegraph);

  const flameGraph = React.useMemo<Flamegraph | null>(() => {
    if(model.length === 0 ) return null;
    const mainName = model[0].stackNames[0];
    const flame: Flamegraph = { name: mainName, value: 0, children: [] };
    model.forEach(row => addNodeToFlamegraph(flame, 1, row));
    return flame;
  }, [model]);

  const isWaitingForData = flameGraph === null;

  return (
    <>
      <Ivette.TitleBar help="eva-flamegraph">
        <IconButton
          icon="PIN"
          kind={useScope ? "positive" : "default"}
          onClick={flipUseScope}
          title={useScope ? "Scope change enabled" : "Scope change disabled"}
        />
        <Inset />
        <EvaStatus />
      </Ivette.TitleBar>
      <EvaReady showChildrenForComputingStatus={!isWaitingForData} >
        {
          !isWaitingForData &&
          <AutoSizer key="flamegraph">
            {(size: Size) => (
              <EvaFlamegraph
                useScope={useScope}
                data={flameGraph}
                size={size}
              />
            )}
          </AutoSizer>
        }
      </EvaReady>
    </>
  );
}

Ivette.registerComponent({
  id: 'fc.eva.flamegraph',
  label: 'Eva Flamegraph',
  title: 'Detailed flamegraph of the Eva analysis',
  children: <FlamegraphComponent />,
});

// --------------------------------------------------------------------------
