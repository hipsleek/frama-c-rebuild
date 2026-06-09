/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React from 'react';
import { Catch } from 'dome/errors';
import { classes } from 'dome/misc/utils';
import { useColor, IHookColors, TColor, EColor } from '../colors';
import { Size } from 'react-virtualized';
import { select, selectAll } from 'd3-selection';
import { graphviz } from 'd3-graphviz';
import AutoSizer from 'react-virtualized-auto-sizer';
import './style.css';

/* -------------------------------------------------------------------------- */
/* --- Graph Specifications                                               --- */
/* -------------------------------------------------------------------------- */

export type Direction = 'LR' | 'TD';

export type Font = 'serif' | 'sans' | 'mono';

export type Shape =
  | 'point' | 'box'
  | 'diamond' | 'hexagon'
  | 'circle' | 'ellipse'
  | 'note' | 'tab' | 'folder' | 'cds';

export type Anchor =
  'n' | 'ne' | 'e' | 'se' | 's' | 'sw' | 'w' | 'nw' | 'c' | '_';

export type Arrow = 'none' | 'arrow' | 'tee' | 'box' | 'dot';

export type Line = 'solid' | 'dashed' | 'dotted';

export type Cell = string | { label: string, port: string };

export type Box = Cell | Box[];

export interface Node {
  /** Node identifier (unique). */
  id: string;
  /** Cluster identifier */
  cluster?: string;
  /** Node label */
  label?: string;
  /** Node tooltip */
  title?: string;
  /** Node font (label) */
  font?: Font;
  /** Node color (filled background) */
  color?: TColor;
  /**
   * Shape. Nested boxes alternate LR and TD directions. Initial direction is
   * orthogonal to the graph direction. Node label is ignored for box layout.
   */
  shape?: Shape | Box[];
}

/**
 *  Edge properties.
 *  Alternative syntax `id:port` is also supported for port names.
 */
export interface Edge {
  /** Source node identifier */
  source: string;
  /** Source port (provided source node has box shape) */
  sourcePort?: string;
  /** Target node identifier */
  target: string;
  /** Target port (provided target node has box shape) */
  targetPort?: string;
  /** Default is `solid` */
  line?: Line;
  /** Default is `dark` */
  color?: TColor;
  head?: Arrow;
  headLabel?: string,
  headAnchor?: Anchor;
  tail?: Arrow;
  tailLabel?: string,
  tailAnchor?: Anchor;
  font?: Font;
  label?: string;
  title?: string;
  /** Edge constraints node placement (default: true). */
  constraint?: boolean;
  /** Node placement on the same rank (default: false). */
  aligned?: boolean;
}

export interface Cluster {
  /** Identifier */
  id: string;
  /** Label (default is none) */
  label?: string;
  /** Title (default is none) */
  title?: string;
  /** Background color (default is grey) */
  color?: TColor;
}

/* -------------------------------------------------------------------------- */
/* --- Graph Component Properties                                         --- */
/* -------------------------------------------------------------------------- */

export interface DiagramProps {
  nodes?: readonly Node[];
  edges?: readonly Edge[];
  clusters?: readonly Cluster[];

  /**
     Element to focus on.
     The default color for this element is `'selected'`.
   */
  selected?: string;

  /** Top-Down (`'TD'`, default) or Left-Right (`'LR'`) direction. */
  direction?: Direction;

  /** Invoked when a node is selected. */
  onSelection?: (node: string | undefined) => void;

  /** Whether the Graph shall be displayed or not (defaults to true). */
  display?: boolean;

  /** Styling the Graph main div element. */
  className?: string;

  /** Debug the generated DotModel */
  onModelChanged?: (model: string) => void;
}

/* -------------------------------------------------------------------------- */
/* --- CSS Model                                                          --- */
/* -------------------------------------------------------------------------- */

const FONTNAME = {
  'serif': 'Times',
  'sans': 'sans-serif',
  'mono': 'Courier',
};

/* -------------------------------------------------------------------------- */
/* --- Edge Model                                                         --- */
/* -------------------------------------------------------------------------- */

const DIR = (h: Arrow, t: Arrow): string | undefined =>
  h === 'none'
    ? (t === 'none' ? 'none' : 'back')
    : (t === 'none' ? undefined : 'both');

/* -------------------------------------------------------------------------- */
/* --- Dot Model                                                          --- */
/* -------------------------------------------------------------------------- */

type cluster = { props: Cluster; nodes: Node[]; }

class Builder {

  private colors: IHookColors;
  private selected: string | undefined;
  private spec = '';

  private kid = 0;
  private imap = new Map<string, string>();
  private rmap = new Map<string, string>();
  private cmap = new Map<string, cluster>();

  constructor(colors: IHookColors) {
    this.colors = colors;
  }

  index(id: string): string {
    const n = this.imap.get(id);
    if (n !== undefined) return n;
    const m = `DNODE_${this.kid++}`;
    this.imap.set(id, m);
    this.rmap.set(m, id);
    return m;
  }

  findCluster(id: string): cluster {
    const c = this.cmap.get(id);
    if (c !== undefined) return c;
    const d = { props: { id }, nodes: [] };
    this.cmap.set(id, d);
    return d;
  }

  addClusterNode(n: Node): void {
    if (n.cluster !== undefined) {
      this.findCluster(n.cluster).nodes.push(n);
    }
  }

  setClusterProps(props: Cluster): void {
    const c = this.findCluster(props.id);
    c.props = props;
  }

  nodeId(n: string): string {
    return this.rmap.get(n) ?? n;
  }

  init(): Builder {
    this.spec = 'digraph {\n';
    this.selected = undefined;
    this.cmap.clear();
    // Keep node index to fade in & out
    return this;
  }

  select(selected: string | undefined): Builder {
    this.selected = selected;
    return this;
  }

  flush(): string { return this.spec.concat('}'); }

  print(...text: string[]): Builder {
    this.spec = this.spec.concat(...text);
    return this;
  }

  println(...text: string[]): Builder {
    this.spec = this.spec.concat(...text).concat('\n');
    return this;
  }

  // --- Attributes

  escaped(a: string): Builder {
    return this.print(a.replace(/["{}|\\]/g, '\\$&'));
  }

  value(a: string | number | boolean): Builder {
    if (typeof a === 'string')
      return this.print('"').escaped(a).print('"');
    else
      return this.print(`${a}`);
  }

  attr(a: string, v: string | number | boolean | undefined): Builder {
    if (v !== undefined && v !== '')
      return this.print(' ', a, '=').value(v).print(';');
    else
      return this;
  }

  // --- Node Table Shape

  port(id: string, port?: string): Builder {
    this.print(this.index(id));
    if (port) this.print(':', this.index(port));
    return this;
  }

  record(r: Box, nested = false): Builder {
    if (Array.isArray(r)) {
      if (nested) this.print('{');
      r.forEach((c, k) => {
        if (k > 0) this.print('|');
        this.record(c, true);
      });
      if (nested) this.print('}');
      return this;
    } else if (typeof r === 'string') {
      return this.escaped(r);
    } else {
      return this.print('<').port(r.port).print('> ').escaped(r.label);
    }
  }

  // --- Node
  node(n: Node): void {
    this
      .print('  ')
      .port(n.id)
      .print(' [')
      .attr('id', n.id)
      .attr('fontname', n.font ? FONTNAME[n.font] : undefined);
    if (typeof n.shape === 'object') {
      this
        .attr('shape', 'record')
        .print(' label="')
        .record(n.shape)
        .print('";')
        .attr('tooltip', n.title ?? n.id);
    } else {
      let label = n.label || n.id;
      if (label && n.shape === 'cds') label += ' ';
      this
        .attr('label', label)
        .attr('shape', n.shape)
        .attr('tooltip', n.title || n.label || n.id);
    }
    const color =
      ( n.id === this.selected
        ? EColor.SELECTED
        : (n.color ?? EColor.DEFAULT) );
    this
      .attr('fontcolor', this.colors.FGCOLOR[color])
      .attr('fillcolor', this.colors.BGCOLOR[color])
      .println(' ];');
  }

  cluster(c: cluster): void {
    const { props: s, nodes } = c;
    const { color = EColor.GREY } = s;
    this
      .print('  subgraph cluster_', this.index(s.id), ' {\n   ')
      .attr('style', 'filled')
      .attr('label', s.label)
      .attr('tooltip', s.title ?? s.id)
      .attr('fontcolor', this.colors.FGCOLOR[color])
      .attr('fillcolor', this.colors.SGCOLOR[color])
      .print('\n   ');
    nodes.forEach(n => this.print(' ', this.index(n.id), ';'));
    this.println('\n  }');
  }

  clusters(cs: readonly Cluster[]): Builder {
    cs.forEach(c => this.setClusterProps(c));
    return this;
  }

  nodes(ns: readonly Node[]): Builder {
    ns.forEach(n => this.addClusterNode(n));
    this.cmap.forEach(c => this.cluster(c));
    ns.forEach(n => this.node(n));
    return this;
  }

  // --- Edge
  edge(e: Edge): void {
    const { line = 'solid', head = 'arrow', tail = 'none' } = e;
    const tooltip = e.title ?? e.label;
    if (e.aligned === true)
      this
        .print('{ rank=same; ')
        .port(e.source).print(' ')
        .port(e.target).println(' };');
    this
      .print('  ')
      .port(e.source, e.sourcePort)
      .print(' -> ')
      .port(e.target, e.targetPort)
      .print(' [')
      .attr('label', e.label)
      .attr('fontname', e.font ? FONTNAME[e.font] : undefined)
      .attr('headport', e.tailAnchor)
      .attr('tailport', e.headAnchor)
      .attr('headlabel', e.headLabel)
      .attr('taillabel', e.tailLabel)
      .attr('constraint', e.constraint === false ? false : undefined)
      .attr('labeltooltip', e.label ? tooltip : undefined)
      .attr('headtooltip', e.headLabel ? tooltip : undefined)
      .attr('tailtooltip', e.tailLabel ? tooltip : undefined)
      .attr('tooltip', tooltip)
      .attr('dir', DIR(head, tail))
      .attr('color', e.color ?
        this.colors.EDCOLOR[e.color] :
        this.colors.EDCOLOR[EColor.DEFAULT])
      .attr('fontcolor', e.color ?
        this.colors.EDCOLOR[e.color] :
        this.colors.EDCOLOR[EColor.DEFAULT])
      .attr('style', line === 'solid' ? undefined : line)
      .attr('arrowhead', head === 'arrow' ? undefined : head)
      .attr('arrowtail', tail === 'arrow' ? undefined : tail)
      .println('];');
  }

  edges(es: readonly Edge[]): Builder {
    es.forEach(e => this.edge(e));
    return this;
  }

}

/* -------------------------------------------------------------------------- */
/* --- d3-Graphviz view                                                   --- */
/* -------------------------------------------------------------------------- */

let divId = 0;
const newDivId = (): string => `dome_xDiagram_g${++divId}`;

interface GraphvizProps extends DiagramProps { size: Size, margin?: number }

function GraphvizView(props: GraphvizProps): JSX.Element {
  // Colors
  const colors = useColor();

  // --- Builder Instance (unique)
  const builder = React.useMemo(() => new Builder(colors), [colors]);

  // --- Model Generation
  const {
    direction = 'LR',
    clusters = [],
    nodes = [],
    edges = [],
    selected
  } = props;

  const model = React.useMemo(() =>
    builder
      .init()
      .select(selected)
      .print(' ')
      .attr('rankdir', direction)
      .attr('bgcolor', 'none')
      .attr('width', 0.5)
      .println('node [ style="filled" ];')
      .clusters(clusters)
      .nodes(nodes)
      .edges(edges)
      .flush()
    , [builder, direction, clusters, nodes, edges, selected]
  );

  // --- Model Update Callback
  const { onModelChanged } = props;
  React.useEffect(() => {
    if (onModelChanged) onModelChanged(model);
  }, [model, onModelChanged]);


  // --- Rendering & Remote
  const [error, setError] = React.useState<string>();
  const id = React.useMemo(newDivId, []);
  const href = `#${id}`;
  const { onSelection } = props;
  const { width, height } = props.size;
  React.useEffect(() => {
    setError(undefined);
    graphviz(href, {
      useWorker: false,
      tweenShapes: false,
      fit: false, zoom: true, width, height,
    }).onerror(setError)
      .renderDot(model).on('end', function () {
        if (onSelection) {
          selectAll('.node')
            .on('click', function (evt: PointerEvent) {
              const s = select(this).attr('id');
              if (s) {
                evt.stopPropagation();
                onSelection(builder.nodeId(s));
              }
            });
        }
      });
  }, [href, model, width, height, builder, onSelection, setError]);

  const onClick = React.useCallback((): void => {
    if (onSelection) onSelection(undefined);
  }, [onSelection]);

  const onKey = React.useCallback((evt: React.KeyboardEvent): void => {
    if (evt.key === 'Escape') {
      evt.preventDefault();
      graphviz(href).resetZoom();
      if (onSelection) onSelection(undefined);
    }
  }, [href, onSelection]);

  if (error !== undefined) throw (error);

  return (
    <div
      id={id}
      tabIndex={-1}
      style={{ outline: 'none' }}
      className={props.className}
      onKeyDown={onKey}
      onClick={onClick} />
  );
}

/* -------------------------------------------------------------------------- */
/* --- Dome Diagram Component                                             --- */
/* -------------------------------------------------------------------------- */

export function Diagram(props: DiagramProps): JSX.Element {
  const { display = true } = props;
  const className = classes('dome-xDiagram', props.className);
  return (
    <>
      {display && (
        <AutoSizer>
          {(size: Size) => (
            <div className={className} style={size}>
              <Catch label='Graphviz Error'>
                <GraphvizView size={size} {...props} />
              </Catch>
            </div>
          )}
        </AutoSizer >
      )}
    </>
  );
}

/* -------------------------------------------------------------------------- */
