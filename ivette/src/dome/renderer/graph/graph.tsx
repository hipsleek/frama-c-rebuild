/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { createRoot } from 'react-dom/client';

import ForceGraph2D, {
  ForceGraphMethods as ForceGraphMethods2D,
  LinkObject as LinkObject2D,
  NodeObject as NodeObject2D,
} from 'react-force-graph-2d';

import ForceGraph3D, {
  ForceGraphMethods as ForceGraphMethods3D,
  LinkObject as LinkObject3D,
  NodeObject as NodeObject3D,
  ForceGraphProps as ForceGraphProps3D,
} from 'react-force-graph-3d';

/** Three is a dependency of 3d-force-graph */
import {
  CSS2DRenderer, CSS2DObject
} from 'three/examples/jsm/renderers/CSS2DRenderer';

import { Size } from 'react-virtualized';
import AutoSizer from 'react-virtualized-auto-sizer';

import * as Dome from 'dome';
import * as Themes from 'dome/themes';
import { transformColor } from '../colors';

/* -------------------------------------------------------------------------- */
/* --- Graph Specifications                                               --- */
/* -------------------------------------------------------------------------- */

/** Type layout */
export type Layout = '2D' | '3D';

export interface Node {
  /** Node identifier (unique). */
  id: string;
  /** Node label (optional). */
  label?: string;
}

export interface Edge {
  source: string /** Source node identifier */;
  target: string /** Target node identifier */;
}

/* -------------------------------------------------------------------------- */
/* --- Force Graph Components                                             --- */
/* -------------------------------------------------------------------------- */

interface GNode {
  id: string;
  label?: string;
  /** Nodes style */
  visible?: boolean;
}
interface GLink {
  source: string;
  target: string;
  /** Links style */
  visible?: boolean;
  color?: string;
  width?: number;
}
interface GData {
  nodes: GNode[];
  links: GLink[];
}

interface IGProps {
  data: GData;
  onSelection?: SelectionCallback;
  selected: string | undefined;
  size: Size;
}

interface IGProps2D extends IGProps {
  options?: IGraphOptions;
}

interface IGProps3D extends IGProps {
  options?: IGraphOptions3D;
}

/* -------------------------------------------------------------------------- */
/* --- Graph options                                                      --- */
/* -------------------------------------------------------------------------- */
/** Nodes options */
export interface INodesOptions {
  /** visibility of the nodes */
  visibility?: boolean | ((node: GNode) => boolean);
}

/** Links options */
export interface ILinksOptions {
  /** Width of the links */
  width?: number | ((node: GLink) => number);
  /** Color of the links */
  color?: string | ((node: GLink) => string);
  /** visibility of the links */
  visibility?: boolean | ((node: GLink) => boolean);
  /** Size of the arrows */
  directionalArrow?: number | ((node: GLink) => number);
  /** Number of directional particles */
  directionalParticle?: number;
  /** Width of directional particles */
  particleWidth?: number | ((node: GLink) => number);
  /** Color of directional particles */
  particleColor?: string | ((node: GLink) => string)
}

/** Common options for Graph2D and Graph3D */
export interface IGraphOptions {
  /** Background color */
  backgroundColor?: string;
  /** Moves the camera to see all of the nodes if active */
  autoCenter?: boolean;
  /**
   * If displayMode = td, the graph will be display like a tree from top to down
   * Work only if the graph has no cycle
   */
  displayMode?: 'td';
  /** Spacing between depths level */
  depthSpacing?: number;
  /**
   * Callback called when a cycle is detected
   * @param val array of cycle node ids
   * */
  onDagError?: (val: string[]) => void;
  /** Nodes options */
  nodesOptions?: INodesOptions;
  /** Links options */
  linkOptions?: ILinksOptions;
}

/** Specific options for graph3D */
export interface IGraphOptions3D extends IGraphOptions {
  /** function to create the HTML for 2D object */
  htmlNode?: (node: NodeObject3D<GNode>) => JSX.Element;
  /** Node repulsion */
  horizontalSpacing?: number;
}

function getOnEngineStop(
  fgRef: React.MutableRefObject<ForceGraphMethods3D<NodeObject3D<GNode>,
  LinkObject3D<GNode, GLink>> | undefined>,
  options: {
    autoCenter?: boolean
  }
):() => void {
  return () => {
    if (options.autoCenter && fgRef.current) {
      fgRef.current.zoomToFit(400, 20);
    }
  };
}

/** Transform JSX.Element to HtmlObject */
const jsxToHtmlObject = (jsxElement: JSX.Element): HTMLDivElement => {
  const container = document.createElement('div');
  createRoot(container).render(jsxElement);
  return container;
};

/** Return a function who make an object2D */
function getObject2DFunction(
  getNode: (node: NodeObject3D<GNode>) => JSX.Element
) {
  return (node: NodeObject3D<GNode>) => {
    const nodeEl = jsxToHtmlObject(getNode(node));
    return new CSS2DObject(nodeEl || document.createElement('div'));
  };
}

/* -------------------------------------------------------------------------- */
/* --- Graph Component Properties                                         --- */
/* -------------------------------------------------------------------------- */

export type Callback = () => void;
export type SelectionCallback = (node: string, evt: MouseEvent) => void;

export interface GraphProps {
  nodes: readonly Node[];
  edges: readonly Edge[];

  /**
     Element to focus on.
     The graph is scrolled to make this node visible if necessary.
   */
  selected?: string;

  /** Layout engine. */
  layout?: Layout;

  /** Invoked when a node is selected. */
  onSelection?: SelectionCallback;

  /** Invoked after layout is computed (typically used after a reset). */
  onReady?: Callback;

  /** Whether the Graph shall be displayed or not (defaults to true). */
  display?: boolean;

  /** Styling the Graph main div element. */
  className?: string;

  /** Options graph 2D */
  options2D?: IGraphOptions;

  /** Options graph 3D */
  options3D?: IGraphOptions3D;
}

/* -------------------------------------------------------------------------- */
/* --- 2D Force Graph Component                                           --- */
/* -------------------------------------------------------------------------- */

function Graph2D(props: IGProps2D): JSX.Element {
  const { data, onSelection, selected, size } = props;
  const { width, height } = size;

  const fgRef2D = React.useRef<
    | ForceGraphMethods2D<NodeObject2D<GNode>, LinkObject2D<GNode, GLink>>
    | undefined
  >(undefined);

  const style = Themes.useStyle();

  React.useEffect(() => {
    if (fgRef2D.current && selected) {
      const selectedNode: NodeObject2D | undefined = data.nodes.find(
        (node) => node.id === selected
      );
      if (selectedNode?.x && selectedNode?.y)
        fgRef2D.current.centerAt(selectedNode.x, selectedNode.y, 500);
    }
  }, [selected, data]);

  return (
    <ForceGraph2D<GNode, GLink>
      ref={fgRef2D}
      width={width}
      height={height}
      nodeId='id'
      nodeLabel='label'
      linkSource='source'
      linkTarget='target'
      graphData={data}
      autoPauseRedraw={true}
      // default value of intensity
      d3AlphaDecay={0.0228}
      dagLevelDistance={50}
      onNodeClick={(node, event): void => {
        if (onSelection) onSelection(node.id, event);
      }}
      // Fix target position on drag end
      onNodeDragEnd={(node) => {
        node.fx = node.x;
        node.fy = node.y;
      }}
      cooldownTime={50}
      nodeColor={(node) => (node.id === selected ?
        style.getPropertyValue('--graph-bg-color-selected') :
        style.getPropertyValue('--graph-bg-color-orange'))
      }
    />
  );
}

/* -------------------------------------------------------------------------- */
/* --- 3D Force Graph Component                                           --- */
/* -------------------------------------------------------------------------- */
function getForceGraphOptions(
  fgRef: React.MutableRefObject<ForceGraphMethods3D<NodeObject3D<GNode>,
  LinkObject3D<GNode, GLink>> | undefined>,
  options: IGraphOptions3D,
  theme: Themes.ColorTheme,
  style: CSSStyleDeclaration
): ForceGraphProps3D<GNode, GLink> {
  const ret: ForceGraphProps3D<GNode, GLink> = {};

  const { linkOptions, nodesOptions } = options;

  const {
    width, color, visibility,
    directionalArrow, directionalParticle,
    particleWidth, particleColor
  } = linkOptions || {};

  ret.linkWidth = width ?? ((link) => link?.width ?? 1);
  ret.linkColor = color ?? ((link) =>
    link?.color ?? style.getPropertyValue('--graph-ed-color-default'));
  ret.linkVisibility = visibility ?? ((link) => link?.visible ?? true);
  ret.linkDirectionalArrowLength = directionalArrow ?? ((link) =>
    ((link?.width ?? 1) * 2));
  ret.linkDirectionalParticles = directionalParticle;
  ret.linkDirectionalParticleWidth = particleWidth ?? ((link) =>
    ((link?.width ?? 1) * 150 / 100));
  ret.linkDirectionalParticleColor = particleColor ?? ((link) => transformColor(
      link?.color ?? "black", theme === "light" ? -50 : 50
    ));

  ret.nodeVisibility = nodesOptions?.visibility ?? ((link) =>
    link?.visible ?? true);

  if(options) {
    const {
      backgroundColor, displayMode, depthSpacing,
      onDagError, htmlNode, autoCenter
    } = options;

    if (displayMode) ret.dagMode = displayMode;
    if (depthSpacing) ret.dagLevelDistance = depthSpacing;
    ret.backgroundColor = backgroundColor ||
      style.getPropertyValue('--background');

    ret.onEngineStop = getOnEngineStop(fgRef, { autoCenter });
    ret.onDagError = (val) => {
      if(typeof val[0] === 'number') {
        // eslint-disable-next-line no-console
        console.error('onDagError : ID must be a string');
      }
      if(onDagError) {
        const newCycle: string[] = [];
        val.forEach((elt) => {
          newCycle.push(typeof elt === 'string' ? elt : elt.toString());
        });
        onDagError(newCycle);
      }
    };

    if(htmlNode) {
      ret.extraRenderers = [new CSS2DRenderer()];
      ret.nodeThreeObjectExtend = false;
      ret.nodeThreeObject = getObject2DFunction(htmlNode);
     }
  }

  return ret;
}

function Graph3D(props: IGProps3D): JSX.Element {
  const { data, onSelection, selected, size,
           options = {}
        } = props;
  const { width, height } = size;

  const fgRef3D = React.useRef<
    | ForceGraphMethods3D<NodeObject3D<GNode>, LinkObject3D<GNode, GLink>>
    | undefined
  >(undefined);

  const [theme,] = Themes.useColorTheme();
  const style = Themes.useStyle();
  const graphOptions = getForceGraphOptions(
    fgRef3D, options || {}, theme, style);

  const [ , flipHorizontalSpacingIsSet ] =
    Dome.useFlipSettings('ivette.callgraph.horizontalSpacingIsSet', true);

  React.useEffect(() => {
    if (fgRef3D.current && selected) {
      // distance to set between camera and node
      const distance = 370;
      const selectedNode: NodeObject3D | undefined = data.nodes.find(
        (node) => node.id === selected
      );
      if (selectedNode) {
        const { x, y, z } = selectedNode;
        if (x && y && z) {
          const distRatio = 1 + distance / Math.hypot(x, y, z);
          fgRef3D.current.cameraPosition(
            // new position
            { x: x * distRatio, y: y * distRatio, z: z * distRatio },
            { x, y, z }, // lookAt Parameter
            1000 // ms transition duration
          );
        }
      }
    }
  }, [fgRef3D, selected, data]);

  React.useEffect(() => {
    /** Waiting for fgRef3D.current initialization */
    if (!fgRef3D.current) {
      const timer = setTimeout(() => {
        flipHorizontalSpacingIsSet();
      }, 100);
      return () => clearTimeout(timer);
    }

    if (options.horizontalSpacing) {
      // Adjust node repulsion
      fgRef3D.current.d3Force('charge')?.strength(-options.horizontalSpacing);
    }
    return;
  }, [options.horizontalSpacing, flipHorizontalSpacingIsSet]);

  return (
    <ForceGraph3D<GNode, GLink>
      ref={fgRef3D}
      width={width}
      height={height}
      nodeId='id'
      nodeLabel='label'
      linkSource='source'
      linkTarget='target'
      graphData={data}
      d3AlphaDecay={0.0228}
      onNodeClick={(node, event): void => {
        if (onSelection) onSelection(node.id, event);
      }}
      cooldownTime={50}
      dagLevelDistance={50}
      controlType='orbit'
      // Fix target position on drag end
      onNodeDragEnd={(node) => {
        node.fx = node.x;
        node.fy = node.y;
        node.fz = node.z;
      }}
      nodeColor={(node) => (node.id === selected ?
        style.getPropertyValue('--graph-bg-color-selected') :
        style.getPropertyValue('--graph-bg-color-orange'))
      }
      {...graphOptions}
    />
  );
}

/* -------------------------------------------------------------------------- */
/* --- Dome Graph Component                                               --- */
/* -------------------------------------------------------------------------- */

export function Graph(props: GraphProps): JSX.Element {
  const { nodes, edges, onSelection,
          display = true, selected, options3D } = props;
  const data: GData = React.useMemo(
    () => ({
      nodes: nodes.slice(),
      links: edges.slice(),
    }),
    [nodes, edges]
  );

  return (
    <>
      {display && props.layout === '2D' && (
        <AutoSizer>
          {(size: Size) => (
            <div className={props.className}>
              <Graph2D
                key='2D'
                data={data}
                onSelection={onSelection}
                selected={selected}
                size={size}
              />
            </div>
          )}
        </AutoSizer>
      )}
      {display && props.layout === '3D' && (
        <AutoSizer>
          {(size: Size) => (
            <div className={props.className}>
              <Graph3D
                key='3D'
                data={data}
                onSelection={onSelection}
                selected={selected}
                options={options3D}
                size={size}
              />
            </div>
          )}
        </AutoSizer>
      )}
    </>
  );
}

/* -------------------------------------------------------------------------- */
