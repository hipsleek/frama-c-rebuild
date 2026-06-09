/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import {
  NodeObject as NodeObject3D,
} from 'react-force-graph-3d';

import * as Ivette from 'ivette';

import * as Dome from 'dome';
import {
  Graph, IGraphOptions3D, ILinksOptions,
} from 'dome/graph/graph';
import { Icon } from 'dome/controls/icons';
import * as Themes from 'dome/themes';
import * as Server from 'frama-c/server';
import { useFunctionFilter } from 'frama-c/kernel/Globals';
import * as Ast from 'frama-c/kernel/api/ast';
import * as Properties from 'frama-c/kernel/api/properties';
import * as States from 'frama-c/states';
import * as EvaAst from 'frama-c/plugins/eva/api/ast';
import * as EvaStats from 'frama-c/plugins/eva/api/stats';
import {
  CGNode, CGLink, CGData,
  SelectedNodes,
  getIDFromLink,
  getNodeVisibility,
  getSuccessor,
  getPredecessors,
  onNodeClickMultiSelect,
  getLinkWidth,
  getLinkColor,
  getLinkVisibility,
  ModeDisplay,
  useSelectedNodes
} from "frama-c/plugins/callgraph/definitions";

import './callgraph.css';
import * as Node from "./components/node";
import { Panel } from './components/panel';
import { CallgraphToolsBar } from "./components/toolbar";
import { useDMButton, useTSButton } from "./components/buttons";
import { CallgraphTitleBar, docCallgraph } from "./components/titlebar";

import * as CgAPI from './api';

// --------------------------------------------------------------------------
// --- Graph functions
// --------------------------------------------------------------------------

function convertGraph(
  graph: CgAPI.graph | undefined,
  functionStats: EvaStats.functionStatsData[],
  properties: Properties.statusData[],
  evaps: EvaAst.propertiesData[],
): CGData
{
  const nodes: CGNode[] = [];
  const links: CGLink[] = [];

  const getScopeTaint = (id: Ast.decl): States.Tag[] => {
    const taint: States.Tag[] = [];

    properties.filter((elt) => elt.scope === id).forEach((elt) => {
      const n = evaps.find((ps) => ps.key === elt.key);
      taint.push(({ name: n?.taint || "not_computed" }));
    });
    return taint;
  };

  if (graph) {
    for (const v of graph.vertices) {
      const stats = functionStats.find((elt) => elt.key === v.decl);
      const scopeTaint = getScopeTaint(v.decl);
      const node: CGNode = {
        id: v.decl,
        label: v.name,
        alarmCount: stats?.alarmCount,
        alarmStatuses: stats?.alarmStatuses,
        coverage: stats?.coverage,
        taintStatus: scopeTaint
      };
      nodes.push(node);
    }
    for (const e of graph.edges) {
      // Check if is recursive function
      if (e.src === e.dst) {
        nodes[nodes.findIndex((elt) => elt.id === e.src)].isRecursive = true;
      } else {
        const link: CGLink = { source: e.src, target: e.dst };
        links.push(link);
      }
    }
  }
  return { nodes, links };
}

function getFilteredGraph(graph: CGData, ids: string[] = []): CGData {
  return {
    nodes: graph.nodes.filter(elt => ids.includes(elt.id)),
    links: graph.links.filter(elt => {
      const { sourceId, targetId } = getIDFromLink(elt);
      return Boolean(ids.includes(sourceId) && ids.includes(targetId));
    })
  };
}

function getStyledGraph(
  displayMode: ModeDisplay,
  predecessors: string[],
  successors: string[],
  selectedNodes: SelectedNodes,
  style: CSSStyleDeclaration,
  linkThickness: number,
  graph: CGData,
): CGData {
  return {
    nodes: graph.nodes.map(node => ({
      ...node,
      visible: getNodeVisibility(
        graph.links,
        displayMode,
        node.id,
        predecessors, successors
      )
    })),
    links: graph.links.map(link => {
      return {
        ...link,
        visible: getLinkVisibility(link, displayMode, predecessors, successors),
        color: getLinkColor(link, selectedNodes, style),
        width: getLinkWidth(link, selectedNodes, linkThickness),
      };
    })
  };
}

/* -------------------------------------------------------------------------- */
/* --- Callgraph component                                                --- */
/* -------------------------------------------------------------------------- */

function Callgraph(): JSX.Element {
  const isComputed = States.useSyncValue(CgAPI.isComputed);
  if(isComputed === false) Server.send(CgAPI.compute, null);

  /** Current location */
  const { scope } = States.useCurrentLocation();

  /** Style */
  const style = Themes.useStyle();

  /** data */
  const graph = States.useSyncValue(CgAPI.callgraph);
  const alarms = States.useSyncArrayData(EvaStats.functionStats);

  /** Function list and properties */
  const functions = States.useSyncArrayData(Ast.functions);
  const properties = States.useSyncArrayData(Properties.status);
  const evaps = States.useSyncArrayData(EvaAst.properties);
  const {
    contextFctFilter, multipleSelection, showFunction } = useFunctionFilter();

  const filteredFunctions =  React.useMemo(() => {
    return functions.filter(showFunction);
  }, [functions, showFunction]);

  /** Control */
  const [ displayMode, setDisplayMode ] = useDMButton();
  const [ selectedParents, setSelectedParents ] =
    useTSButton('selectedparents');
  const [ selectedChildren, setSelectedChildren ] =
    useTSButton('selectedChildren');

  const showParticlesState = Dome.useFlipState(true);
  const [ showParticles, flipShowParticles ] = showParticlesState;
  const panelVisibleState =
    Dome.useFlipSettings("ivette.callgraph.panelVisible", true);
  const [ verticalSpacing, setVerticalSpacing ] =
    Dome.useNumberSettings("ivette.callgraph.verticalspacing", 75);
  const [ horizontalSpacing, setHorizontalSpacing ] =
    Dome.useNumberSettings("ivette.callgraph.horizontalspacing", 500);
  const [ linkThickness, setLinkThickness ] =
    Dome.useNumberSettings("ivette.callgraph.linkThickness", 1);
  const [ autoCenter, flipAutoCenter ] =
    Dome.useFlipSettings("ivette.callgraph.autocenter", true);
  const [ autoSelect, flipAutoSelect ] =
    Dome.useFlipSettings('ivette.callgraph.autoselect', true);

  /** Specific nodes*/
  const selectedFunctions = React.useMemo<Set<string>>(() => {
    if(!multipleSelection) return new Set();
    return new Set(multipleSelection as string[]);
  }, [multipleSelection]);

  const taintedFunctions =  React.useMemo(() => {
    const scope: string[] = [];
    evaps.forEach((ps) => {
      if(ps.taint === "direct_taint" || ps.taint === "indirect_taint") {
        const prop = properties.find((elt) => elt.key === ps.key);
        if(prop && prop.scope && !scope.includes(prop.scope))
          scope.push(prop.scope);
      }
    });
    return scope;
  }, [properties, evaps]);

  const unprovenPropertiesFunctions = React.useMemo<Set<string>>(() => {
    const ids: SelectedNodes = new Set();
    alarms.forEach(elt => {
      if (elt.alarmCount.length > 0) ids.add(elt.key);
    });
    return ids;
  }, [alarms]);

  /** Graph */
  const graphData = React.useMemo<CGData>(() => {
    return convertGraph(graph, alarms, properties, evaps);
  }, [graph, alarms, properties, evaps]);

  const filteredGraph = React.useMemo<CGData>(() => {
    return getFilteredGraph(graphData, filteredFunctions.map(elt => elt.decl));
  }, [graphData, filteredFunctions]);

  /** Selected */
  const selectedNodesState = useSelectedNodes();
  const [selectedNodes, setSelectedNodes] = selectedNodesState;

  /** Predecessors of all selected nodes */
  const predecessors = React.useMemo(() => {
    return getPredecessors(filteredGraph.links, selectedNodes, selectedParents);
  }, [filteredGraph, selectedNodes, selectedParents]);

  /** Successors of all selected nodes */
  const successors = React.useMemo(() => {
    return getSuccessor(filteredGraph.links, selectedNodes, selectedChildren);
  }, [filteredGraph, selectedNodes, selectedChildren]);

  /** Filtered and styled graph */
  const filteredAndStyledGraph = React.useMemo<CGData>(() => {
    return getStyledGraph(
      displayMode, predecessors, successors, selectedNodes,
      style, linkThickness, filteredGraph );
  }, [filteredGraph, displayMode, linkThickness, predecessors, successors,
      selectedNodes, style]);

  const getNode = React.useMemo(() => {
    return (node: NodeObject3D<CGNode>): React.JSX.Element => {
      return Node.getNode(node, selectedNodesState, onNodeClickMultiSelect);
    };
  }, [selectedNodesState]);

  /** Count visible links and nodes */
  const [ visibleNodes, setVisibleNodes ] = React.useState(0);
  const [ visibleLinks, setVisibleLink ] = React.useState(0);
  React.useEffect(() => {
    /** Calculating the number of visible nodes and links
     * has to wait for rendering, so we add a timeout. */
    const timeout = setTimeout(() => {
      setVisibleNodes(filteredAndStyledGraph.nodes.filter(
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        node => (node as any).__threeObj?.visible).length);
      const countLinks = filteredAndStyledGraph.links.filter(
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        link => (link as any).__lineObj?.visible).length;
      setVisibleLink(countLinks);
      if(countLinks > 1000) flipShowParticles(false);
      }, 100);

    return () => clearTimeout(timeout);
  }, [filteredAndStyledGraph, displayMode, flipShowParticles,
      selectedNodes, selectedParents, selectedChildren]
  );

  React.useEffect(() => {
    if(autoSelect && scope)
      setSelectedNodes([scope]);
  }, [scope, autoSelect, setSelectedNodes]);

  React.useEffect(() => {
    if(autoSelect && selectedFunctions.size > 0)
      setSelectedNodes(selectedFunctions);
  }, [selectedFunctions, autoSelect, setSelectedNodes]);

  const cycles = React.useRef<string[][]>([]);

  const onDagError = (val: string[]): void => {
    const isAlreadySave = (): boolean => {
      for (const i in cycles.current) {
        if (val.length === cycles.current[i].length) {
          for( const j in cycles.current[i] ) {
            if (cycles.current[i][j] !== val[j]) break;
          }
          return true;
        }
      }
      return false;
    };
    if(!isAlreadySave()) {
      cycles.current.push(val);
      setSelectedNodes(cycles.current.flat());
    }
  };

  const linkOptions: ILinksOptions = {
    directionalParticle: showParticles ? 3 : 0,
  };

  const options3D: IGraphOptions3D = {
    backgroundColor: style.getPropertyValue('--background'),
    autoCenter: autoCenter,
    displayMode: 'td',
    depthSpacing: verticalSpacing,
    horizontalSpacing: horizontalSpacing,
    onDagError,
    htmlNode: getNode,
    linkOptions
  };

  return (
    <>
      <CallgraphTitleBar
        contextFctFilter={contextFctFilter}
        autoCenterState={[ autoCenter, flipAutoCenter ]}
        autoSelectState={[ autoSelect, flipAutoSelect ]}
      />
      <CallgraphToolsBar
        displayModeState={[ displayMode, setDisplayMode ]}
        selectedParentsState={[ selectedParents, setSelectedParents ]}
        selectedChildrenState={[ selectedChildren, setSelectedChildren ]}
        panelVisibleState={panelVisibleState}
        verticalSpacingState={[ verticalSpacing, setVerticalSpacing ]}
        horizontalSpacingState={[ horizontalSpacing, setHorizontalSpacing ]}
        linkThicknessState={[ linkThickness, setLinkThickness ]}
        showParticlesState={showParticlesState}
        selectedFunctions={selectedFunctions}
        taintedFunctions={taintedFunctions}
        unprovenPropertiesFunctions={unprovenPropertiesFunctions}
        cycleFunctions={cycles.current.flat()}
        dagMode={displayMode}
        updateNodes={setSelectedNodes}
      />

      {!isComputed &&
          <Icon
            id={"SPINNER"}
            className={"cg-graph-computing"}
            size={130}
          />
      }

      {isComputed &&
        <div className='cg-graph-container'>
          <Graph
            layout='3D'
            nodes={filteredAndStyledGraph.nodes}
            edges={filteredAndStyledGraph.links}
            selected={undefined}
            options3D={options3D}
          />
          <Panel
            graphData={filteredAndStyledGraph}
            selectedNodes={selectedNodes}
            tainted={taintedFunctions.length}
            properties={properties}
            evaProperties={evaps}
            style={style}
            panelVisibleState={panelVisibleState}
            visibleNodes={visibleNodes}
            visibleLinks={visibleLinks}
          />
        </div>
      }

    </>
  );
}

/* -------------------------------------------------------------------------- */
/* --- Register component                                                 --- */
/* -------------------------------------------------------------------------- */

Ivette.registerComponent({
  id: 'fc.callgraph',
  label: 'Call Graph',
  title:
    'Display a graph showing calls between functions.',
  children: <Callgraph />,
});

Ivette.registerView({
  id: 'fc.callgraph',
  label: 'Callgraph',
  layout: {
    ABCD: 'fc.callgraph',
  }
});

Ivette.registerDocChapter(docCallgraph);

// --------------------------------------------------------------------------
