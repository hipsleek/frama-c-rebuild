/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React, { useState, useEffect, useImperativeHandle, useRef } from 'react';
import _ from 'lodash';
import { renderToString } from 'react-dom/server';
import * as Dome from 'dome';
import * as Ivette from 'ivette';
import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';
import * as Locations from 'frama-c/kernel/Locations';
import * as Ast from 'frama-c/kernel/api/ast';
import * as API from './api';

import Cytoscape from 'cytoscape';
import CytoscapeComponent from 'react-cytoscapejs';
import './cytoscape_libs';

import tippy, * as Tippy from 'tippy.js';
import 'tippy.js/dist/tippy.css';
import 'tippy.js/themes/light-border.css';
import 'tippy.js/animations/shift-away.css';
import './tippy.css';

import { IconButton } from 'dome/controls/buttons';
import { Inset } from 'dome/frame/toolbars';

import { EvaReady, EvaStatus }
  from 'frama-c/plugins/eva/components/AnalysisStatus';
import style from './style.json';
import layouts from './layouts.json';
import './dive.css';
import { Label } from 'dome/controls/labels';
import { Vbox } from 'dome/layout/boxes';
import { SVG } from 'dome/controls/icons';

const Debug = new Dome.Debug('dive');

interface Cxtcommand {
  content: string;
  select: () => void;
  enabled: boolean;
}

interface CytoscapeExtended extends Cytoscape.Core {
  cxtmenu(options: unknown): void;
}

type Identified = { id: string }
type NodeData = Cytoscape.NodeDataDefinition & Identified
type EdgeData = { id: string;[key: string]: unknown }
type NodeOrId = string | Cytoscape.NodeSingular

function callstackToString(callstack: API.callstack): string {
  return callstack.map((cs) => `${cs.fun}:${cs.instr}`).join('/');
}

function buildCxtMenu(
  commands: Cxtcommand[],
  content?: JSX.Element,
  action?: () => void,
): void {
  commands.push({
    content: content ? renderToString(content) : '',
    select: action || (() => { /* Do nothing */ }),
    enabled: !!action,
  });
}

/* double click events for Cytoscape */

function enableDoubleClickEvents(cy: Cytoscape.Core, delay = 350): void {
  let last: Cytoscape.EventObject | undefined;
  cy.on('click', (e) => {
    if (last && last.target === e.target &&
      e.timeStamp - last.timeStamp < delay) {
      e.target.trigger('double-click', e);
    }
    last = e;
  });
}

/* The Dive class handles the selection of nodes according to user actions.
   To prevent cytoscape to automatically select (and unselect) nodes wrongly,
   we make some nodes unselectable. We then use the functions below to make
   the nodes selectable before (un)selecting them. */

function select(node: Cytoscape.NodeSingular): void {
  node.selectify();
  node.select();
}

function unselect(node: Cytoscape.NodeSingular): void {
  node.selectify();
  node.unselect();
}

export type mode = 'explore' | 'overview';

class Dive {
  headless: boolean;
  cy: Cytoscape.Core;
  mode: mode = 'explore';
  _layout = '';
  layoutOptions: Cytoscape.LayoutOptions | undefined;
  currentSelection: string | null = null;
  selectedLocation: (States.Location | undefined) = undefined;

  constructor(cy: Cytoscape.Core | null = null) {
    this.cy = cy || Cytoscape();
    this.headless = this.cy.container() === null;
    this.cy.elements().remove();

    this.cy.minZoom(1e-1);
    this.cy.maxZoom(1.0);

    // Remove previous listeners
    this.cy.off('click');
    this.cy.off('double-click');

    // Add new listeners
    enableDoubleClickEvents(this.cy);
    this.cy.on('click', 'node', (event) => this.clickNode(event.target));
    this.cy.on('click', 'edge', (event) => this.clickEdge(event.target));
    this.cy.on('double-click', '$node > node', // compound nodes
      (event) => this.doubleClickNode(event.target));

    // Default layout
    this.layout = 'dagre';

    // Contextual menu
    if (!this.headless) {
      this.cy.scratch('cxtmenu')?.destroy?.(); // Remove previous menu
      this.cy.scratch('cxtmenu', (this.cy as CytoscapeExtended).cxtmenu({
        selector: 'node',
        commands: (node: Cytoscape.NodeSingular) => this.onCxtMenu(node),
      }));
    }

    // Node width hack
    const padding = 10;
    const min = 50;
    /* eslint-disable @typescript-eslint/no-explicit-any */
    if (this.cy.style() && !this.headless) {
      const canvas = document.querySelector('canvas[data-id="layer2-node"]');
      if (canvas instanceof HTMLCanvasElement) {
        const context = canvas.getContext('2d');
        if (context) {
          (this.cy.style() as any).selector('node').style('width',
            (node: any) => {
              const fStyle = node.pstyle('font-style').strValue;
              const weight = node.pstyle('font-weight').strValue;
              const size = node.pstyle('font-size').pfValue;
              const family = node.pstyle('font-family').strValue;
              context.font = `${fStyle} ${weight} ${size}px ${family}`;
              const width = context.measureText(node.data('label')).width;
              return `${Math.max(min, width + padding)}px`;
            });
        }
      }
    }
    /* eslint-enable @typescript-eslint/no-explicit-any */
  }

  onCxtMenu(node: Cytoscape.NodeSingular): Cxtcommand[] {
    const data = node.data();
    const commands = [] as Cxtcommand[];
    buildCxtMenu(commands,
      <Vbox>
        <SVG id='BINOCULARS' fill='#f8f8ff' className='dive-svg'/>
        <Label label='Explore' />
      </Vbox>,
      () => { this.explore(node); });
    if (data.nkind === 'composite')
      buildCxtMenu(commands,
        <Vbox>
          <SVG id='ARROWS.CROSSED' fill='#f8f8ff' className='dive-svg'/>
          <Label label='Unfold' />
        </Vbox>
      );
    else
      buildCxtMenu(commands);
    if (data.backward_explored === 'no')
      buildCxtMenu(commands,
        <Vbox>
          <SVG id='SHOW.ON' fill='#f8f8ff' className='dive-svg'/>
          <Label label='Show' />
        </Vbox>,
        () => this.show(node));
    else
      buildCxtMenu(commands,
        <Vbox>
          <SVG id='SHOW.OFF' fill='#f8f8ff' className='dive-svg'/>
          <Label label='Hide' />
        </Vbox>,
        () => this.hide(node));
    return commands;
  }

  remove(node: Cytoscape.NodeSingular): void {
    const parent = node.parent();
    node.remove();
    this.cy.$id(`${node.id()}-more`).remove();
    if (parent.nonempty() && parent.children().empty())
      this.remove(parent as Cytoscape.NodeSingular); // Recursively remove parents
  }

  referenceFile(fileName: string): Cytoscape.NodeSingular {
    const id = `file_${fileName}`;
    const node = this.cy.$id(id);
    if (node.nonempty()) {
      return node;
    }

    const nodeDefinition = {
      data: { id, label: fileName },
      classes: 'file',
      pannable: true,
    };
    // cytoscape.add type declaration is missing the 'pannable' field
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    return this.cy.add(nodeDefinition);
  }

  referenceCallstack(callstack: API.callstack): Cytoscape.NodeSingular | null {
    const name = callstackToString(callstack);
    const elt = callstack.shift();

    if (!elt)
      return null;

    const id = `callstack_${name}`;
    const node = this.cy.$id(id);
    if (node.nonempty()) {
      return node;
    }

    const parentNode = this.referenceCallstack(callstack);
    const nodeDefinition = {
      data: { id, label: elt.fun, parent: parentNode?.id() },
      classes: 'function',
      pannable: true
    };
    // cytoscape.add type declaration is missing the 'pannable' field
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    return this.cy.add(nodeDefinition);
  }

  createTips(node: Cytoscape.NodeSingular): Tippy.Instance[] {
    const container = this.cy.container();
    if (!container)
      return [];

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const ref = (node as any).popperRef();
    const common = {
      getReferenceClientRect: ref.getBoundingClientRect,
      interactive: true,
      animation: 'shift-away',
      duration: 500,
      trigger: 'manual',
      appendTo: document.body,
    };

    const tips = [];

    if (node.data().values) {
      tips.push(tippy(container, {
        ...common,
        content: node.data().values,
        placement: 'top',
        offset: [0, 20],
        arrow: true,
      }));
    }

    if (node.data().type) {
      tips.push(tippy(container, {
        ...common,
        content: node.data().type,
        placement: 'bottom',
        offset: [0, 20],
        theme: 'light-border',
        arrow: false,
      }));
    }

    return tips;
  }

  addTips(node: Cytoscape.NodeSingular): void {
    let timeout: NodeJS.Timeout;
    let tips: Tippy.Instance[] | null = null;

    node.on('mouseover', () => {
      if (tips === null) {
        tips = this.createTips(node);
        tips.forEach((tip) => tip.props.onHidden = (() => tip.destroy()));
      }
      clearTimeout(timeout);
      timeout = setTimeout(() => tips?.forEach((tip) => {
        tip.show();
      }), 200);
    });

    node.on('mouseout', () => {
      clearTimeout(timeout);
      timeout = setTimeout(() => {
        tips?.forEach((tip) => tip.hide());
        tips = null; // Force rebuilding tips in case they changed
      }, 0);
    });
  }

  updateElement(group: Cytoscape.ElementGroup, data: NodeData | EdgeData):
    Cytoscape.CollectionReturnValue {
    let element = this.cy.$id(data.id);
    if (element.nonempty()) {
      element.removeData();
      element.data(data);
    }
    else {
      element = this.cy.add({ group, data });
      element.addClass("new");
    }
    element.removeClass("stale");
    return element;
  }

  updateNode(data: NodeData):
    Cytoscape.NodeSingular {
    const element = this.updateElement("nodes", data);
    element.addClass('node');
    return element;
  }

  updateEdge(src: NodeOrId, dst: NodeOrId, data: EdgeData):
    Cytoscape.EdgeSingular {
    const source = typeof src === "string" ? src : src.id();
    const target = typeof dst === "string" ? dst : dst.id();
    data = { ...data, source, target };
    const element = this.updateElement("edges", data);
    element.addClass('dependency');
    return element;
  }

  updateNodeData(node: API.node): void {
    const data = { ...node, id: `${node.id}` } as NodeData;

    // Interval range visualization (see cytoscape stops style property)
    if (typeof node.range === 'number')
      data.stops = `0% ${node.range}% ${node.range}% 100%`;

    // Build clusters for this node if needed
    if (node.locality.callstack)
      data.parent = this.referenceCallstack(node.locality.callstack)?.id();
    else
      data.parent = this.referenceFile(node.locality.file).id();

    // Add new node or update existing node
    const ele = this.updateNode(data);
    if (ele.hasClass("new"))
      this.addTips(ele);

    // Add a node for the user to ask for more dependencies
    const idmore = `${node.id}-more`;
    if (node.backward_explored === 'partial') {
      const elemore = this.updateNode({ id: idmore, parent: data.parent });
      elemore.addClass("more");
      this.updateEdge(elemore, ele, { id: `e${node.id}-more` });
    }
    else {
      this.cy.$id(idmore).remove();
    }
  }

  updateEdgeData(edge: API.dependency): void {
    const data = { ...edge, id: `e${edge.id}` } as EdgeData;
    this.updateEdge(`${edge.src}`, `${edge.dst}`, data);
  }

  updateGraph(data: API.graphData[]): void {
    this.cy.startBatch();
    this.cy.$('.node, .dependency').addClass('stale');

    // Update vertices
    for (const d of data) {
      if ('nkind' in d.element) // Node
        this.updateNodeData(d.element);
    }

    // Edges must be updated after vertices since their sources and destination
    // must have been created beforehand
    for (const d of data) {
      if ('dkind' in d.element) // Dependency
        this.updateEdgeData(d.element);
    }

    // Remove nodes that are not present anymore
    this.cy.$('.stale').forEach(n => this.remove(n));

    this.cy.endBatch();

    this.recomputeLayout(this.cy.$('node.new'));
  }

  get layout(): string {
    return this._layout;
  }

  set layout(layout: string) {
    if (layout === this._layout)
      return;
    let extendedOptions = {};
    if (layout in layouts)
      extendedOptions = (layouts as { [key: string]: object })[layout];
    this._layout = layout;
    this.layoutOptions = {
      name: layout,
      ...extendedOptions,
    };

    this.recomputeLayout();
  }

  recomputeLayout(newNodes?: Cytoscape.Collection): void {
    if (this.layoutOptions && this.cy.container() &&
      (newNodes === undefined || !newNodes.empty())) {
      this.cy.layout({
        animationEasing: 'ease-in-out-quad',
        /* Do not move new nodes */
        animateFilter: (node: Cytoscape.Singular) => newNodes === undefined ||
          !newNodes.contains(node),
        stop: () => {
          this.cy.$('.new').addClass('old').removeClass('new');
        },
        ...this.layoutOptions,
      } as unknown as Cytoscape.LayoutOptions).run();
    }
  }

  async exec<In>(
    request: Server.ExecRequest<In, number | undefined | null>,
    param: In): Promise<Cytoscape.NodeSingular | undefined> {
    try {
      if (Server.isRunning()) {
        await this.setMode();
        const r = await Server.send(request, param);
        if (r) {
          return undefined;
        }
      }
    }
    catch (err) {
      Debug.error(err);
    }

    return undefined;
  }

  static async setWindow(window: API.explorationWindow): Promise<void> {
    if (Server.isRunning())
      await Server.send(API.window, window);
  }

  async setMode(): Promise<void> {
    switch (this.mode) {
      case 'explore':
        await Dive.setWindow({
          perception: { backward: 3, forward: 1 },
          horizon: { backward: 3, forward: 1 },
        });
        break;
      case 'overview':
        await Dive.setWindow({
          perception: { backward: 4, forward: 1 },
          horizon: { backward: undefined, forward: undefined },
        });
        break;
      default: /* This is useless and impossible if the program is correctly
                  typed, but the linter wants it */
    }
  }

  clear(): void {
    this.cy.elements().remove();
    this.exec(API.clear, null);
  }

  async add(marker: string): Promise<void> {
    const node = await this.exec(API.add, marker);
    if (node)
      this.updateNodeSelection(node);
  }

  async explore(node: Cytoscape.NodeSingular): Promise<void> {
    const id = parseInt(node.id(), 10);
    if (id)
      await this.exec(API.explore, id);
  }

  show(node: Cytoscape.NodeSingular): void {
    const id = parseInt(node.id(), 10);
    if (id)
      this.exec(API.show, id);
  }

  hide(node: Cytoscape.NodeSingular): void {
    const id = parseInt(node.id(), 10);
    if (id)
      this.exec(API.hide, id);
  }

  async clickNode(node: Cytoscape.NodeSingular): Promise<void> {
    // Node selection
    this.updateNodeSelection(node);
    await this.explore(node);
    // Update locations
    const label = node.data()?.label as (string | undefined);
    const markers = node.data()?.writes as Ast.marker[];
    if (label && markers) {
      Locations.setNextSelection({
        plugin: 'Dive',
        label: `Dive: writes to ${label}`,
        title: 'Selected writes from Dive current selection',
        markers
      });
    }

    /* Cytoscape automatically selects the node clicked, and unselects all other
       nodes and edges. As we want some incoming edges to remain selected, we
       make the node unselectable, preventing cytoscape to select it. */
    node.unselectify();
  }

  async clickEdge(edge: Cytoscape.EdgeSingular): Promise<void> {
    // Unselect everything
    this.cy.$(':selected').forEach(unselect);
    this.cy.$('.multiple-selection').removeClass('multiple-selection');
    this.cy.$('.selection').removeClass('selection');
    // Update locations
    const markers = edge.data()?.origins as Ast.marker[];
    if (markers) {
      Locations.setNextSelection({
        plugin: `Dive`,
        label: `Dive: origins`,
        title: 'Origins of current edge in Dive graph',
        markers
      });
    }
  }

  doubleClickNode(node: Cytoscape.NodeSingular): void {
    this.cy.animate({ fit: { eles: node, padding: 10 } });
  }

  selectLocation(
    location: States.Location | undefined,
    doExplore: boolean): void {
    if (!location) {
      // Reset whole graph if no location is selected.
      this.clear();
    } else if (location !== this.selectedLocation) {
      this.selectedLocation = location;
      const selectNode = this.cy.$('node:selected');
      const writes = selectNode?.data()?.writes;
      if (doExplore && location.marker && !_.some(writes, location)) {
        this.add(location.marker);
      }
      else {
        this.updateNodeSelection(selectNode);
      }
    }
  }

  updateNodeSelection(node: Cytoscape.NodeSingular): void {
    const hasOrigin = (ele: Cytoscape.NodeSingular): boolean => (
      _.some(ele.data().origins, this.selectedLocation)
    );
    this.cy.$(':selected').forEach(unselect);
    this.cy.$('.multiple-selection').removeClass('multiple-selection');
    this.cy.$('.selection').removeClass('selection');
    select(node);
    const edges = node.incomers('edge');
    const relevantEdges = edges.filter(hasOrigin);
    edges.addClass('multiple-selection');
    relevantEdges.addClass('selection');
  }
}


type GraphViewProps = {
  addSelection: boolean;
  grabbable: boolean;
  layout: string;
  selectionMode: string;
}

type GraphViewRef = {
  clear: () => void;
}

const GraphView = React.forwardRef<GraphViewRef | undefined, GraphViewProps>(
  (props: GraphViewProps, ref) => {
    const { addSelection, grabbable, layout, selectionMode } = props;

    const [dive, setDive] = useState(() => new Dive());
    const selection = States.useCurrentLocation();
    const graph = States.useSyncArrayData(API.graph);

    function setCy(cy: Cytoscape.Core): void {
      if (cy !== dive.cy)
        setDive(new Dive(cy));
    }

    useImperativeHandle(ref, () => ({ clear: () => dive.clear() }));

    useEffect(() => {
      setDive(new Dive(dive.cy)); // On hot reload, setup a new instance
    }, [Dive]); // eslint-disable-line react-hooks/exhaustive-deps

    useEffect(() => {
      dive.layout = layout;
    }, [dive, layout]);

    useEffect(() => {
      dive.updateGraph(graph);
    }, [dive, graph]);

    // Follow mode
    useEffect(() => {
      dive.mode = selectionMode === 'follow' ? 'explore' : 'overview';
    }, [dive, selectionMode]);

    // Updates the graph according to the selected marker.
    useEffect(() => {
      dive.selectLocation(selection, addSelection);
    }, [dive, addSelection, selection]);

    return (
      <CytoscapeComponent
        stylesheet={style}
        cy={setCy}
        autoungrabify={!grabbable}
        style={{ width: '100%', height: '100%' }}
      />);
  });

GraphView.displayName = "GraphView";


export default function GraphComponent(): JSX.Element {
  const graph = useRef<GraphViewRef>();
  const [addSelection, flipAddSelection] =
    Dome.useFlipSettings('dive.addSelection', true);
  const [grabbable, flipGrabbable] =
    Dome.useFlipSettings('dive.grabbable', true);
  const [selectionMode, setSelectionMode] =
    Dome.useStringSettings('dive.selectionMode', 'follow');
  const [layout, setLayout] =
    Dome.useStringSettings('dive.layout', 'dagre');

  // Selection mode
  const selectMode = (id?: string) => void (id && setSelectionMode(id));
  const modes = [
    { id: 'follow', label: 'Follow selection' },
    { id: 'add', label: 'Add selection to the graph' },
  ];
  const checkMode =
    (item: { id: string; label: string }): Dome.PopupMenuItem => (
      { checked: item.id === selectionMode, ...item }
    );
  const modeMenu = (): void => {
    Dome.popupMenu(modes.map(checkMode), selectMode);
  };

  // Layout selection
  const selectLayout = (layout?: string) => void (layout && setLayout(layout));
  const layoutsNames = ['cose-bilkent', 'dagre', 'cola', 'klay'];
  const layoutItem = (id: string): Dome.PopupMenuItem =>
    ({ id, label: id, checked: (id === layout) });
  const layoutMenu = () =>
    void Dome.popupMenu(layoutsNames.map(layoutItem), selectLayout);

  // Component
  return (
    <>
      <Ivette.TitleBar help="dive">
        <IconButton
          icon="PIN"
          onClick={flipAddSelection}
          kind={addSelection ? 'positive' : 'negative'}
          title={
            addSelection
              ? 'Do not add selected AST elements into the graph'
              : 'Add selected AST elements into the graph'
          }
        />
        <IconButton
          icon="LOCK"
          onClick={flipGrabbable}
          kind={grabbable ? 'positive' : 'negative'}
          title={
            grabbable ? 'Disallow nodes to be moved' : 'Allow nodes to be moved'
          }
        />
        <IconButton
          icon="SETTINGS"
          onClick={modeMenu}
          title="Choose the selection mode"
        />
        <IconButton
          icon="DISPLAY"
          onClick={layoutMenu}
          title="Choose the graph layout"
        />
        <Inset />
        <IconButton
          icon="TRASH"
          onClick={() => graph.current?.clear()}
          title="Clear the graph"
        />
        <Inset />
        <EvaStatus />
        <Inset />
      </Ivette.TitleBar>
      <EvaReady>
        <GraphView
          addSelection={addSelection}
          grabbable={grabbable}
          layout={layout}
          selectionMode={selectionMode}
          ref={graph}
        />
      </EvaReady>
    </>
  );

}

// --------------------------------------------------------------------------
