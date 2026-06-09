/* ************************************************************************ */
/*                                                                          */
/*   SPDX-License-Identifier LGPL-2.1                                       */
/*   Copyright (C)                                                          */
/*   CEA (Commissariat à l'énergie atomique et aux énergies alternatives)   */
/*                                                                          */
/* ************************************************************************ */

import React, { useEffect } from 'react';

import { Icon } from 'dome/controls/icons';
import { classes } from 'dome/misc/utils';
import { IconButton } from 'dome/controls/buttons';
import { Inset } from 'dome/frame/toolbars';
import * as Ivette from 'ivette';

import * as Server from 'frama-c/server';
import * as States from 'frama-c/states';

import { EvaReady, EvaStatus }
  from 'frama-c/plugins/eva/components/AnalysisStatus';
import * as API from './api';
import type { marker } from 'frama-c/kernel/api/ast';

import './dive.css';

const w = {
  perception: { backward: 3, forward: 0 },
  horizon: { backward: undefined, forward: undefined },
};

async function exec<I, O>(rq: Server.ExecRequest<I, O>, i: I):
  Promise<O | undefined> {
  if (Server.isRunning()) {
    await Server.send(API.window, w);
    return await Server.send(rq, i);
  }

  return undefined;
}

async function requestLocation(location: States.Location):
  Promise<number | undefined> {
  return await exec(API.add, location.marker);
}

async function explore(node: API.node): Promise<null | undefined> {
  return await exec(API.explore, node.id);
}

function isDependency(el: API.element): el is API.dependency {
  return 'dst' in el;
}

interface FolderProps {
  unfolded: boolean;
  onclick?: () => void
}

function Folder(props: FolderProps): JSX.Element {
  return <IconButton
    icon={props.unfolded ? 'MINUS' : 'PLUS'}
    title="Fold / Unfold the dependencies"
    className="folder"
    onClick={props.onclick}
  />;
}

function Exploring(): JSX.Element {
  return (
  <>
    <Icon
      id={"SPINNER"}
      className={"exploration"}
      size={130}
    />
    Exploring...
  </>
  );
}

interface MarkerProps { marker: marker }

function Marker(props: MarkerProps): JSX.Element {
  const { marker } = props;
  const { descr } = States.useMarker(marker);
  const current = States.useSelected();
  const hovered = States.useHovered();
  const classeName = classes(
    'marker',
    current === marker && 'selected',
    hovered === marker && 'hovered'
  );
  return (
    <span
      className={classeName}
      onClick={() => States.setSelected(marker)}
      onMouseEnter={() => States.setHovered(marker)}
      onMouseLeave={() => States.setHovered(undefined)}
    >
      {descr}
    </span>
  );
}

type WithKey<P> = P & { key: string | number | null }

type WithItemChildren<P> =
  P & {
    children?: WithKey<React.ReactElement> | WithKey<React.ReactElement>[]
  }

type TreeNodeProps = {
  label: React.ReactNode,
  className?: string,
  unfolded?: boolean,
  onunfold?: () => void,
}

function TreeNode(props: WithItemChildren<TreeNodeProps>): JSX.Element {
  const [unfolded, setUnfolded] = React.useState(props.unfolded === true);
  const children =
    props.children ?
      (Array.isArray(props.children) ? props.children : [props.children]) :
      [];

  const toggle = (): void => {
    unfolded === false && props.onunfold && props.onunfold();
    setUnfolded(!unfolded);
  };

  return <div className={props.className}>
    <span className="label">
      {children.length > 0 ?
        <Folder unfolded={unfolded} onclick={toggle} /> : <></>
      }
      {props.label}
    </span>
    {unfolded ?
      <ul>
        {children.map((element) => <li key={element.key}>{element}</li>)}
      </ul> :
      null
    }
  </div>;
}

function MarkerNode(props: WithItemChildren<{ marker: marker }>):
  JSX.Element {
  return <TreeNode
    unfolded={true}
    label={<Marker marker={props.marker} />}
    className="marker">
    {props.children}
  </TreeNode>;
}

function GraphNode(props: { nodeId: number, unfolded?: boolean }): JSX.Element {
  const graphData = States.useSyncArrayElt(API.graph, `n${props.nodeId}`);
  const graph = States.useSyncArrayData(API.graph);

  if (graphData && 'label' in graphData.element) {
    const node = graphData.element;
    const deps = graph
      .map((data) => data.element)
      .filter(isDependency)
      .filter((d) => d.dst === node.id);

    // Transform dependencies into a map 'statement' -> 'memory location read
    // at this statement'
    const map = new Map<marker, Set<API.nodeId>>();
    deps.forEach((d) => {
      d.origins.forEach((marker) => {
        let entry = map.get(marker);
        if (entry === undefined) {
          entry = new Set();
          map.set(marker, entry);
        }
        entry.add(d.src);
      });
    });

    return <TreeNode
      unfolded={props.unfolded}
      onunfold={() => explore(node)}
      label={node.label}>
      {node.backward_explored ?
        Array.from(map.entries()).map(([m, sources]) =>
          <MarkerNode marker={m} key={m}>
            {Array.from(sources.values()).map((src) =>
              <GraphNode nodeId={src} key={src} />
            )}
          </MarkerNode>
        ) :
        <Exploring key={null} />
      }
    </TreeNode>;
  }

  return <>Error while building the tree</>;
}

export default function TreeComponent(): JSX.Element {
  const [root, setRoot] = React.useState<number | null>(null);
  const current = States.useCurrentLocation();

  useEffect(() => {
    const update = async (): Promise<void> => {
      if (current.marker) {
        const node = await requestLocation(current);
        root === null && node !== null && node !== undefined && setRoot(node);
      }
    };
    update();
  }, [current, root]);

  return <>
    <Ivette.TitleBar help="dive-dataflow-tree">
      <IconButton
        icon="TRASH"
        onClick={() => setRoot(null)}
        title="Clear the graph"
      />
      <Inset />
      <EvaStatus />
    </Ivette.TitleBar>
    <EvaReady>
      {
        root === null ?
          <>Select an expression to investigate</>
          :
          <div className="diveTree">
            <GraphNode nodeId={root} unfolded={true} />
          </div>
      }
    </EvaReady>
  </>;
}
